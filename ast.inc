/*

WARNING!!!

These macros cannot have commas in the struct members definition,
otherwise the macro will be interpreted as having more parameters
(they should have 2). That is why we put every member in a different
line and use only semicolons as separators!

*/

AST(IntLiteral,    { int val; })
AST(FloatLiteral,  { float val; })
AST(DoubleLiteral, { double val; })

AST(Label, { 
   Atom *atom;
})

AST(BinOp, {
   OpType  op;
   Node   *left;
   Node   *right;
})

AST(LocalVar, {
   Atom *atom;
   Node *init;
})

AST(GlobalVar, {
   Atom *atom;
   Node *init;
})

AST(Block, { 
   Array *nodes; 
})

AST(IfStmt, { 
   Node *cond;
   Node *then;
   Node *els; 
})

AST(ForStmt, { 
   Node *before;
   Node *cond;
   Node *after;
   Node *block; 
})

AST(WhileStmt, { 
   Node *cond;
   Node *block;
})
