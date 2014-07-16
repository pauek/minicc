#ifndef AST_H
#define AST_H

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

struct NodeList : public AstNode {
   std::vector<AstNode*> _children;

   int      num_children() const { return _children.size(); }
   AstNode* child(int n)         { return _children[n]; }
   void     add(AstNode* n)      { _children.push_back(n); }
   void     visit(AstVisitor* v);
};

typedef NodeList Program;

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

struct FuncDecl : public AstNode {
   struct Param {
      Type *type;
      std::string name;
      CommentNode *c[3];
   };

   Type *return_type;
   std::string name;
   std::vector<Param> params;
   Stmt* block;
   
   FuncDecl(std::string _name) : name(_name) {}
   void visit(AstVisitor *v);
};

struct Expr;

struct Stmt : public AstNode {
   enum Type { _empty, _expr, _block, _for, _while, _if, _switch };

   Type type;
   Expr* expr; 
   std::vector<Stmt*> sub_stmts;

   Stmt(Type _type = _empty) : type(_type) {}
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

struct AstVisitor {
   virtual void visit_comment(CommentNode*) = 0;
   virtual void visit_nodelist(Program*) = 0;
   virtual void visit_include(Include*) = 0;
   virtual void visit_macro(Macro *) = 0;
   virtual void visit_using(Using *) = 0;
   virtual void visit_funcdecl(FuncDecl *) = 0;
   virtual void visit_type(Type *) = 0;
   virtual void visit_stmt(Stmt *) = 0;
   virtual void visit_expr(Expr *) = 0;
};

// Visit implementations
inline void NodeList::visit(AstVisitor *v)    { v->visit_nodelist(this); }
inline void CommentNode::visit(AstVisitor *v) { v->visit_comment(this); }
inline void Include::visit(AstVisitor *v)     { v->visit_include(this); }
inline void Macro::visit(AstVisitor *v)       { v->visit_macro(this); }
inline void Using::visit(AstVisitor *v)       { v->visit_using(this); }
inline void FuncDecl::visit(AstVisitor* v)    { v->visit_funcdecl(this); }
inline void Type::visit(AstVisitor *v)        { v->visit_type(this); }
inline void Stmt::visit(AstVisitor *v)        { v->visit_stmt(this); }
inline void Expr::visit(AstVisitor *v)        { v->visit_expr(this); }

#endif
