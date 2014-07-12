#ifndef AST_H
#define AST_H

#include <string>
#include <vector>

class AstNode {
public:
   virtual ~AstNode() {}
   virtual int num_children()    const { return 0; }
   virtual AstNode* child(int n) const { return 0; }
};

class VectorNode : public AstNode {
   std::vector<AstNode*> _children;
public:
   int num_children() const { return _children.size(); }
   AstNode* child(int n)    { return _children[n]; }

   void add(AstNode* n) { _children.push_back(n); }
};

typedef VectorNode Program;

class Include : public AstNode {
   std::string _file;
public:
   Include(std::string file) : _file(file) {}
};

#endif
