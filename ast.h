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

      void  ast_print(AST_Node *node);
const char *op2str(OpType op);

extern int indent_size;


#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)


const char *op2str(OpType op) {
   switch (op) {
   case OP_ASSIGN: return "=";  break;
   case OP_EQUALS: return "=="; break;
   default:
      return "<?>";
   }
}

struct AST_PrintState {
   int level;
};

#define MAX_INDENT 1024

int indent_size = 3;

char *ast__indent(int level) {
   static char indent_str[MAX_INDENT] = {};
   assert((level * indent_size) < MAX_INDENT);
   char *curr = indent_str;
   for (int i = 0; i < level; i++) {
      for (int j = 0; j < indent_size; j++) {
         *curr++ = ' ';
      }
   }
   *curr = 0;
   return indent_str;
}

void ast__print(AST_PrintState* state, Buffer *B, AST_Node *node) {
   if (node == 0) {
      buf_printf(B, "<>");
      return;
   }

#define CASE(T) case AST_##T: { AST_ACCESS(it, T, node);
#define END     break; }

   switch (node->tag) {
   CASE(IntLiteral)    buf_printf(B, "%d", it->val); END
   CASE(FloatLiteral)  buf_printf(B, "%f", it->val); END
   CASE(DoubleLiteral) buf_printf(B, "%e", it->val); END
   CASE(Label)
      buf_printf(B, "%s%s:\n", ast__indent(state->level-1), it->atom->str);
   END
   CASE(ForStmt)
      buf_printf(B, "%sfor (", ast__indent(state->level));
      ast__print(state, B, it->before);
      buf_printf(B, "; ");
      ast__print(state, B, it->cond);
      buf_printf(B, "; ");
      ast__print(state, B, it->after);
      buf_printf(B, ") ");
      ast__print(state, B, it->block);
   END
   CASE(LocalVar)
      buf_printf(B, "%s", it->atom->str);
   END
   CASE(Block)
      buf_printf(B, "{\n");
      state->level++;
      for (size_t i = 0; i < array_len(it->nodes); i++) {
         ast__print(state, B, *(AST_Node **)array_get(it->nodes, i));
      }
      state->level--;
      buf_printf(B, "%s}\n", ast__indent(state->level));
   END
   CASE(BinOp)
      // @Incorrect: this should be done for expression statements not for binops
      buf_printf(B, "%s", ast__indent(state->level));
      ast__print(state, B, it->left);
      buf_printf(B, " %s ", op2str(it->op));
      ast__print(state, B, it->right);
      buf_printf(B, ";\n");
   END
   default:
      buf_printf(B, "<unknown>");
   }

#undef CASE
#undef END

}

void ast_print(Buffer *B, AST_Node *node) {
   AST_PrintState state = { 0 };
   ast__print(&state, B, node);
}


void ast_test() {
   Array *stmts = array_new(0, sizeof(AST_Node *));
   AST_Node *block = ast_node((Block){ stmts });

   AST_Node *label = ast_node((Label){ atom_get("blah", 4) });
   array_push(stmts, &label);
   AST_Node *i = ast_node((IntLiteral){ 5 });
   array_push(stmts, &i);

   AST_Node *assign = ast_node((BinOp){ 
      OP_ASSIGN, 
      ast_node((LocalVar){ atom_get("a", 1), 0 }),
      ast_node((BinOp){
         OP_EQUALS,
         ast_node((LocalVar){ atom_get("b", 1), 0 }),
         ast_node((FloatLiteral){ 1.4f })
      })
   });
   array_push(stmts, &assign);

   Array *stmts2 = array_new(0, sizeof(AST_Node *)); 
   AST_Node *f = ast_node((ForStmt){ 0, 0, 0, ast_node((Block){ stmts2 }) });
   array_push(stmts, &f);

   Buffer *b = buf_new();
   ast_print(b, block);
   printf("[%d, %d] %s", b->len, b->avail, b->str);
   buf_free(b);
}


#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
