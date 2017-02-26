/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)


enum AST_Type {
#define AST(type, members) AST_##type,
#include "ast.inc"
#undef  AST
};

// sizeof(Node) is the size of the member 'type' + padding to 
// start the member 'data' aligned.
struct AST_Node {
   AST_Type type;
   uint8_t  data[];
};

#define AST_NEW(T, x) do {\
   x = (AST_Node *)malloc(sizeof(AST_Node) + sizeof(T));  assert(x); \
   x->type = AST_##T; \
} while(0)

#define AST(type, members) struct type members;
#include "ast.inc"
#undef  AST

#define AST_CAST(T, node) ((T *)&node->data[0])
#define AST_ACCESS(var, T, node) assert(node->type == AST_##T); T *var = AST_CAST(T, node);

#define AST(type, members) \
   AST_Node *ast_node(type data) { AST_Node *n; AST_NEW(type, n); *AST_CAST(type, n) = data; return n; }
#include "ast.inc"
#undef  AST

#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)


void ast_test() {
   AST_Node *f = ast_node((ForStmt){ 0, 0, 0 });
   AST_ACCESS(_while, WhileStmt, f);
   _while->cond = (AST_Node*)1;
}


#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
