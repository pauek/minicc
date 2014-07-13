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
   std::vector<CommentNode*> comments;
   virtual ~AstNode() {}
   virtual int num_children()    const { return 0; }
   virtual AstNode* child(int n) const { return 0; }
   virtual void visit(AstVisitor* v) = 0;
};

struct Comment {
   enum Type { singleline, multiline };
   Type type;
   std::string text;

   Comment(Type t) : type(t) {}
};

struct CommentNode : public AstNode {
   std::vector<Comment> comments;
   void visit(AstVisitor* v);
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

   void visit(AstVisitor* v);
};

// AstVisitor

struct AstVisitor {
   virtual void visit_comment(CommentNode*) = 0;
   virtual void visit_nodelist(Program*) = 0;
   virtual void visit_include(Include*) = 0;
   virtual void visit_macro(Macro *) = 0;
   virtual void visit_using(Using *) = 0;
};

// Visit implementations

inline void CommentNode::visit(AstVisitor* v) { v->visit_comment(this); }
inline void Include::visit(AstVisitor* v)     { v->visit_include(this); }
inline void Macro::visit(AstVisitor* v)       { v->visit_macro(this); }
inline void Using::visit(AstVisitor* v)       { v->visit_using(this); }

#endif
