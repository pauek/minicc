/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)


enum AST_Tag {
   AST_None = 0,
#define AST(type, members) AST_##type,
#include "ast.inc"
#undef  AST
   AST_TotalNodes,
};

enum OpType {
   OP_ASSIGN,
   OP_EQUALS,
};

// sizeof(Node) is the size of 'tag' & 'type' + padding to
// start the member 'data' aligned.
struct AST_Node {
   AST_Tag tag  = AST_None;
   Type   *type = NULL;
   uint8_t data[];
};

#define AST_NEW(T, x) do {\
   x = (AST_Node *)malloc(sizeof(AST_Node) + sizeof(T));  assert(x); \
   x->tag = AST_##T; \
} while(0)

#define AST(type, members) struct type members;
#include "ast.inc"
#undef  AST

#define AST_CAST(T, node) ((T *)&node->data[0])
#define AST_ACCESS(var, T, node) assert(node->tag == AST_##T); T *var = AST_CAST(T, node);

#define AST(type, members) \
   AST_Node *ast_node(type data) { \
      AST_Node *n; \
      AST_NEW(type, n); \
      *AST_CAST(type, n) = data; \
      return n; \
   }
#include "ast.inc"
#undef  AST


void ast_show(AST_Node *node);
const char *op2str(OpType op);


#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)


void ast_test() {
   AST_Node *f = ast_node((ForStmt){ 0, 0, 0, 0 });
   AST_Node *i = ast_node((IntLiteral){ 5 });
   AST_Node *label = ast_node((Label){ atom_get("blah", 4) });

   Array *stmts = array_new(sizeof(AST_Node *), 0);
   AST_Node *block = ast_node((Block){ stmts });

   AST_Node *assign = ast_node((BinOp){ 
      OP_ASSIGN, 
      ast_node((LocalVar){ atom_get("a", 1), 0 }),
      ast_node((BinOp){
         OP_EQUALS,
         ast_node((LocalVar){ atom_get("b", 1), 0 }),
         ast_node((FloatLiteral){ 1.4f })
      })
   });

   ast_show(assign);
   ast_show(label);
   ast_show(f);
}

const char *op2str(OpType op) {
   switch (op) {
   case OP_ASSIGN: return "=";  break;
   case OP_EQUALS: return "=="; break;
   default:
      return "<?>";
   }
}

void ast_show(AST_Node *node) {
   if (node == 0) {
      printf("<>");
      return;
   }

#define CASE(T) case AST_##T: { AST_ACCESS(it, T, node);
#define END     break; }

   switch (node->tag) {
   CASE(IntLiteral)    printf("%d", it->val); END
   CASE(FloatLiteral)  printf("%f", it->val); END
   CASE(DoubleLiteral) printf("%e", it->val); END
   CASE(Label)
      printf("%s:", it->atom->str);
   END
   CASE(ForStmt)
      printf("(for ");
      ast_show(it->before);
      printf(" ");
      ast_show(it->cond);
      printf(" ");
      ast_show(it->after);
      printf(" ");
      ast_show(it->block);
      printf(")");
   END
   CASE(LocalVar)
      printf("%s", it->atom->str);
   END
   CASE(BinOp)
      printf("(%s ", op2str(it->op));
      ast_show(it->left);
      printf(" ");
      ast_show(it->right);
      printf(")");
   END
   default:
      printf("<unknown>");
   }

#undef CASE
#undef END

}


#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
