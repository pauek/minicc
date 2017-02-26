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
   OpType    op;
   AST_Node *left;
   AST_Node *right;
})

AST(LocalVar, {
   Atom     *atom;
   AST_Node *init;
})

AST(GlobalVar, {
   Atom     *atom;
   AST_Node *init;
})

AST(Block, { 
   Array *nodes; 
})

AST(IfStmt, { 
   AST_Node *cond;
   AST_Node *then;
   AST_Node *els; 
})

AST(ForStmt, { 
   AST_Node *before;
   AST_Node *cond;
   AST_Node *after;
   AST_Node *block; 
})

AST(WhileStmt, { 
   AST_Node *cond;
   AST_Node *block;
})


