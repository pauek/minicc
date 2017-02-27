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
   Tag     tag;
   Type   *type;
   uint8_t data[];
};

#define   AST(type, members) struct t##type members;
#include "ast.inc"
#undef    AST

#define AST_ACCESS(var, T, node) \
   assert(node->tag == T); \
   t##T *var = ((t##T *)node->data);

#define AST(T, members) \
   Node *_node(t##T data) { \
      Node *n = (Node *)malloc(sizeof(Node) + sizeof(t##T)); \
      assert(n); \
      n->tag = T; \
      *((t##T *)n->data) = data; \
      return n; \
   }
#include "ast.inc"
#undef  AST

inline Node *_if_(Node *cond, Node *then, Node *els = 0) {
   return _node((tIfStmt){ cond, then, els });
}
inline Node *_while_(Node *cond, Node *block) {
   return _node((tWhileStmt){ cond, block });
}
inline Node *_for_(Node *bef, Node *cond, Node *aft, Node *block) {
   return _node((tForStmt){ bef, cond, aft, block });
}
inline Node *_binop_(OpType op, Node *left, Node *right) {
   return _node((tBinOp){ op, left, right });
}
inline Node *_globalvar_(Atom *atom, Node *init = 0) {
   return _node((tGlobalVar){ atom, init });
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
inline Node *_double_(double f) {
   return _node((tDoubleLiteral){ f });
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

const int MAX_INDENT = 1024;

int indent_size = 3;

inline bool is_control(Node *node) {
   return node->tag == IfStmt || node->tag == WhileStmt || node->tag == ForStmt;
}

inline bool has_semicolon(Node *node) {
   return !is_control(node) && node->tag != Block && node->tag != Label;
}

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
      printf(B, "%s:\n", it->atom->str);
   END
   CASE(IfStmt)
      printf(B, "if (");
      _print(state, B, it->cond);
      printf(B, ") ");
      _print(state, B, it->then);
      if (it->els) {
         printf(B, "else ");
         _print(state, B, it->els);
      }
   END
   CASE(WhileStmt)
      printf(B, "while (");
      _print(state, B, it->cond);
      printf(B, ") ");
      _print(state, B, it->block);
   END
   CASE(ForStmt)
      printf(B, "for (");
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
   CASE(GlobalVar)
      printf(B, "%s", it->atom->str);
   END
   CASE(Block)
      printf(B, "{\n");
      state->level++;
      for (size_t i = 0; i < array::len(it->nodes); i++) {
         printf(B, "%s", _indent(state->level));
         Node *sub = *(Node **)array::get(it->nodes, i);
         _print(state, B, sub);
         if (has_semicolon(sub)) {
            printf(B, ";");
         }
         printf(B, "\n");
      }
      state->level--;
      printf(B, "%s}", _indent(state->level));
   END
   CASE(BinOp)
      _print(state, B, it->left);
      printf(B, " %s ", op2str(it->op));
      _print(state, B, it->right);
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
   printf(B, "\n"); // @Hack: do this well
}


void test() {
   using atom::atom;

   Array *stmts = array::make(0, sizeof(Node *));
   Node *bl = _block_(stmts);

   Node *lab = _label_(atom("blah"));
   array::push(stmts, &lab);
   Node *i = _int_(5);
   array::push(stmts, &i);

   Node *a = _localvar_(atom("a"));
   Node *b = _localvar_(atom("b"));

   Node *assign = _binop_(OP_ASSIGN, a, _binop_(OP_EQUALS, b, _float_(1.4f)));
   array::push(stmts, &assign);

   Array *stmts2 = array::make(0, sizeof(Node *)); 
   Node *f = _for_(0, 0, 0, _block_(stmts2));
   array::push(stmts, &f);

   Array *stmts3 = array::make(0, sizeof(Node *));
   Node *ii = _if_(
      _binop_(OP_EQUALS, a, _int_(7)),
      _block_(stmts3)
   );

   Node *assign2 = _binop_(OP_ASSIGN, a, _int_(10));
   array::push(stmts3, &assign2);

   array::push(stmts, &ii);

   Buffer *buf = buf::make();
   print(buf, bl);
   printf("[%d, %d] %s", buf->len, buf->avail, buf->str);
   buf::free(buf);
}

}

#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
