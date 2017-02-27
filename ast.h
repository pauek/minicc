/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)

namespace ast {

enum Tag {
   None = 0,
#define AST(type, members) type,
#include "ast.inc"
#undef  AST
   TotalNodes,
};

enum OpType {
   OP_ASSIGN,
   OP_EQUALS,
};

// sizeof(Node) is the size of 'tag' & 'type' + padding to
// start the member 'data' aligned.
struct Node {
   Tag     tag  = None;
   Type   *type = NULL;
   uint8_t data[];
};

#define AST_NEW(T, x) do {\
   x = (Node *)malloc(sizeof(Node) + sizeof(T));  assert(x); \
   x->tag = T; \
} while(0)

#define   AST(type, members) struct t##type members;
#include "ast.inc"
#undef    AST

#define AST_CAST(T, node) ((t##T *)&node->data[0])
#define AST_ACCESS(var, T, node) assert(node->tag == T); t##T *var = AST_CAST(T, node);

#define AST(T, members) \
   Node *_node(t##T data) { \
      Node *n; \
      AST_NEW(T, n); \
      *AST_CAST(T, n) = data; \
      return n; \
   }
#include "ast.inc"
#undef  AST


inline Node *_for_(Node *bef, Node *cond, Node *aft, Node *block) {
   return _node((tForStmt){ bef, cond, aft, block });
}
inline Node *_binop_(OpType op, Node *left, Node *right) {
   return _node((tBinOp){ op, left, right });
}
inline Node *_localvar_(Atom *atom, Node *init = 0) {
   return _node((tLocalVar){ atom, init });
}
inline Node *_int_(int i) {
   return _node((tIntLiteral){ i });
}
inline Node *_float_(float f) {
   return _node((tFloatLiteral){ f });
}
inline Node *_label_(Atom *atom) {
   return _node((tLabel){ atom });
}
inline Node *_block_(Array *stmts) {
   return _node((tBlock){ stmts });
}

      void  print(Node *node);
const char *op2str(OpType op);

extern int indent_size;

}

#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)


namespace ast {

const char *op2str(OpType op) {
   switch (op) {
   case OP_ASSIGN: return "=";  break;
   case OP_EQUALS: return "=="; break;
   default:
      return "<?>";
   }
}

struct PrintState {
   int level;
};

#define MAX_INDENT 1024

int indent_size = 3;

static char *_indent(int level) {
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

static void _print(PrintState* state, Buffer *B, Node *node) {
   using buf::printf;

   if (node == 0) {
      printf(B, "<>");
      return;
   }

#define CASE(T) case T: { AST_ACCESS(it, T, node);
#define END     break; }

   switch (node->tag) {
   CASE(IntLiteral)    printf(B, "%d", it->val); END
   CASE(FloatLiteral)  printf(B, "%f", it->val); END
   CASE(DoubleLiteral) printf(B, "%e", it->val); END
   CASE(Label)
      printf(B, "%s%s:\n", _indent(state->level-1), it->atom->str);
   END
   CASE(ForStmt)
      printf(B, "%sfor (", _indent(state->level));
      _print(state, B, it->before);
      printf(B, "; ");
      _print(state, B, it->cond);
      printf(B, "; ");
      _print(state, B, it->after);
      printf(B, ") ");
      _print(state, B, it->block);
   END
   CASE(LocalVar)
      printf(B, "%s", it->atom->str);
   END
   CASE(Block)
      printf(B, "{\n");
      state->level++;
      for (size_t i = 0; i < array::len(it->nodes); i++) {
         _print(state, B, *(Node **)array::get(it->nodes, i));
      }
      state->level--;
      printf(B, "%s}\n", _indent(state->level));
   END
   CASE(BinOp)
      // @Incorrect: this should be done for expression statements not for binops
      printf(B, "%s", _indent(state->level));
      _print(state, B, it->left);
      printf(B, " %s ", op2str(it->op));
      _print(state, B, it->right);
      printf(B, ";\n");
   END
   default:
      printf(B, "<unknown>");
   }

#undef CASE
#undef END

}

void print(Buffer *B, Node *node) {
   PrintState state = { 0 };
   _print(&state, B, node);
}


void test() {
   using atom::atom;

   Array *stmts = array::make(0, sizeof(Node *));
   Node *b = _block_(stmts);

   Node *lab = _label_(atom("blah"));
   array::push(stmts, &lab);
   Node *i = _int_(5);
   array::push(stmts, &i);

   Node *assign = _binop_( 
      OP_ASSIGN, 
      _localvar_(atom("a")),
      _binop_(OP_EQUALS, _localvar_(atom("b")), _float_(1.4f))
   );
   array::push(stmts, &assign);

   Array *stmts2 = array::make(0, sizeof(Node *)); 
   Node *f = _for_(0, 0, 0, _block_(stmts2));
   array::push(stmts, &f);

   Buffer *buf = buf::make();
   print(buf, b);
   printf("[%d, %d] %s", buf->len, buf->avail, buf->str);
   buf::free(buf);
}

}

#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
