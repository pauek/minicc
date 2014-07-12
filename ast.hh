#ifndef AST_H
#define AST_H

#include <string>
#include <vector>

class AstVisitor;

struct AstNode {
   virtual ~AstNode() {}
   virtual int num_children()    const { return 0; }
   virtual AstNode* child(int n) const { return 0; }
   virtual void visit(AstVisitor* v) = 0;
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

// AstVisitor

struct AstVisitor {
   virtual void visit_nodelist(Program*) = 0;
   virtual void visit_include(Include*) = 0;
   virtual void visit_macro(Macro *) = 0;
};

inline void Include::visit(AstVisitor* v) { 
   v->visit_include(this); 
}

inline void Macro::visit(AstVisitor* v) { 
   v->visit_macro(this); 
}

#endif
