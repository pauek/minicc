/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)

namespace ast {

enum Tag {
   None = 0,
#define AST(T, members) tag##T,
#include "ast.inc"
#undef  AST
   TotalNodes,
};

enum OpType {
   OP_BARBAR,
   OP_AMPAMP,
   OP_OR,
   OP_AND,

   OP_EQEQ,
   OP_NOTEQ,
   OP_LT,
   OP_GT,
   OP_LEQ,
   OP_GEQ,

   OP_NOT,
   OP_AMP,
   OP_BAR,
   OP_XOR,
   OP_PLUS,
   OP_MINUS,
   OP_STAR,
   OP_SLASH,
   OP_MOD,
   OP_LSHIFT,
   OP_RSHIFT,
   OP_INC,
   OP_DEC,
   OP_POST_INC,
   OP_POST_DEC,

   OP_EQ,
   OP_PLUSEQ,
   OP_MINUSEQ,
   OP_STAREQ,
   OP_SLASHEQ,
   OP_MODEQ,
   OP_AMPEQ,
   OP_BAREQ,
   OP_XOREQ,
   OP_LSHIFTEQ,
   OP_RSHIFTEQ,

   OP_ERROR,
};

// sizeof(Node) is the size of 'tag' & 'type' + padding to
// start the member 'data' aligned.
struct Node {
   Tag     tag;
   Type   *type;
   uint8_t data[];
};

// declaration of structures
#define   AST(T, members) struct type##T members;
#include "ast.inc"
#undef    AST

#define AST_ACCESS(var, T, node) \
   assert(node->tag == tag##T); \
   type##T *var = ((type##T *)node->data);

// _node functions dispatched for every type of structure
#define AST(T, members) \
   Node *_node(type##T data) { \
      Node *n = (Node *)malloc(sizeof(Node) + sizeof(type##T)); \
      assert(n); \
      n->tag = tag##T; \
      *((type##T *)n->data) = data; \
      return n; \
   }
#include "ast.inc"
#undef  AST

// interface

      void  print(Node *node);
const char *op2str(OpType op);
    OpType  str2op(const char *s_op);

extern int indent_size;

inline Node *If(Node *cond, Node *then, Node *els = 0) {
   return _node((typeIfStmt){ cond, then, els });
}
inline Node *While(Node *cond, Node *block) {
   return _node((typeWhileStmt){ cond, block });
}
inline Node *For(Node *bef, Node *cond, Node *aft, Node *block) {
   return _node((typeForStmt){ bef, cond, aft, block });
}
inline Node *UnOp(const char *s_op, Node *operand) {
   return _node((typeUnOp){ str2op(s_op), operand });
}
inline Node *BinOp(Node *left, const char *s_op, Node *right) {
   return _node((typeBinOp){ str2op(s_op), left, right });
}
inline Node *GlobalVar(Atom *atom, Node *init = 0) {
   return _node((typeGlobalVar){ atom, init });
}
inline Node *LocalVar(Atom *atom, Node *init = 0) {
   return _node((typeLocalVar){ atom, init });
}
inline Node *Literal(int i) {
   return _node((typeIntLiteral){ i });
}
inline Node *Literal(float f) {
   return _node((typeFloatLiteral){ f });
}
inline Node *Literal(double f) {
   return _node((typeDoubleLiteral){ f });
}
inline Node *Label(Atom *atom) {
   return _node((typeLabel){ atom });
}
inline Node *Block(Array *stmts) {
   return _node((typeBlock){ stmts });
}
inline Node *Block() {
   Array *stmts = array::make(0, sizeof(Node *));
   return _node((typeBlock){ stmts });
}
Node *Block(Node *first, ...) {
   // @Speed: we should count arguments first, and create an array with that size??
   Array *stmts = array::make(0, sizeof(Node *));
   va_list args;
   va_start(args, first);
   int i = 0;
   array::push(stmts, &first);
   loop {
      Node *n = va_arg(args, Node *);
      if (n == 0) {
         break;
      }
      array::push(stmts, &n);
      i++;
   }
   va_end(args);
   return _node((typeBlock){ stmts });
}
#define BlockBegin  Block(
#define BlockEnd    0)

inline void push(Node *node, Node *child) {
   AST_ACCESS(block, Block, node);
   array::push(block->nodes, &child);
}

inline Node *Equals(Node *left, Node *right) {
   return BinOp(left, "==", right);
}
inline Node *Assign(Node *left, Node *right) {
   return BinOp(left, "=", right);
}

}

#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)


namespace ast {

const char *op2str(OpType op) {
   switch (op) {
   case OP_BARBAR:     return "||"; break;
   case OP_AMPAMP:     return "&&"; break;
   case OP_OR:         return "or"; break;
   case OP_AND:        return "and"; break;
   case OP_EQEQ:       return "=="; break;
   case OP_NOTEQ:      return "!="; break;
   case OP_LT:         return "<"; break;
   case OP_GT:         return ">"; break;
   case OP_LEQ:        return "<="; break;
   case OP_GEQ:        return ">="; break;
   case OP_NOT:        return "!"; break;
   case OP_AMP:        return "&"; break;
   case OP_BAR:        return "|"; break;
   case OP_XOR:        return "^"; break;
   case OP_PLUS:       return "+"; break;
   case OP_MINUS:      return "-"; break;
   case OP_STAR:       return "*"; break;
   case OP_SLASH:      return "/"; break;
   case OP_MOD:        return "%"; break;
   case OP_LSHIFT:     return "<<"; break;
   case OP_RSHIFT:     return ">>"; break;
   case OP_INC:        return "++"; break;
   case OP_DEC:        return "--"; break;
   case OP_POST_INC:   return "++"; break;
   case OP_POST_DEC:   return "--"; break;
   case OP_EQ:         return "="; break;
   case OP_PLUSEQ:     return "+="; break;
   case OP_MINUSEQ:    return "-="; break;
   case OP_STAREQ:     return "*="; break;
   case OP_SLASHEQ:    return "/="; break;
   case OP_MODEQ:      return "%="; break;
   case OP_AMPEQ:      return "&="; break;
   case OP_BAREQ:      return "|="; break;
   case OP_XOREQ:      return "^="; break;
   case OP_LSHIFTEQ:   return "<<="; break;
   case OP_RSHIFTEQ:   return ">>="; break;
   default:
      return "<?>";
   }
}

#define int2s(s)   (uint8_t(s[0])*256 + uint8_t(s[1]))
#define int2(a, b) (uint8_t(a)*256    + uint8_t(b))

OpType str2op(const char *s_op) {
   switch (strlen(s_op)) {
   case 1:
      switch (s_op[0]) {
      case '<': return OP_LT;
      case '>': return OP_GT;
      case '!': return OP_NOT;
      case '&': return OP_AMP;
      case '|': return OP_BAR;
      case '^': return OP_XOR;
      case '+': return OP_PLUS;
      case '-': return OP_MINUS;
      case '*': return OP_STAR;
      case '/': return OP_SLASH;
      case '%': return OP_MOD;
      case '=': return OP_EQ;
      }
      break;
   case 2:
      switch (int2s(s_op)) {
      case int2('|', '|'): return OP_BARBAR;
      case int2('&', '&'): return OP_AMPAMP;
      case int2('o', 'r'): return OP_OR;
      case int2('=', '='): return OP_EQEQ;
      case int2('!', '='): return OP_NOTEQ;
      case int2('<', '='): return OP_LEQ;
      case int2('>', '='): return OP_GEQ;
      case int2('<', '<'): return OP_LSHIFT;
      case int2('>', '>'): return OP_RSHIFT;
      case int2('+', '+'): return OP_INC;
      case int2('-', '-'): return OP_DEC;
      case int2('+', '='): return OP_PLUSEQ;
      case int2('-', '='): return OP_MINUSEQ;
      case int2('*', '='): return OP_STAREQ;
      case int2('/', '='): return OP_SLASHEQ;
      case int2('%', '='): return OP_MODEQ;
      case int2('&', '='): return OP_AMPEQ;
      case int2('|', '='): return OP_BAREQ;
      case int2('^', '='): return OP_XOREQ;
      }
      break;
   case 3:
      if (0 == strncmp(s_op, "and", 3)) {
         return OP_AND;
      } else if (0 == strncmp(s_op, "<<=", 3)) {
         return OP_LSHIFTEQ;
      } else if (0 == strncmp(s_op, ">>=", 3)) {
         return OP_RSHIFTEQ;
      } else if (0 == strncmp(s_op, "_++", 3)) {
         return OP_POST_INC;
      } else if (0 == strncmp(s_op, "_--", 3)) {
         return OP_POST_DEC;
      }
   }
   return OP_ERROR;
}
   

struct PrintState {
   int level;
};

const int MAX_INDENT = 1024;

int indent_size = 3;

inline bool is_control(Node *node) {
   return node->tag == tagIfStmt || node->tag == tagWhileStmt || node->tag == tagForStmt;
}

inline bool has_semicolon(Node *node) {
   return !is_control(node) && node->tag != tagBlock && node->tag != tagLabel;
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

#define CASE(T) case tag##T: { AST_ACCESS(it, T, node);
#define END     break; }

   switch (node->tag) {
   CASE(IntLiteral)    printf(B, "%d",  it->val); END
   CASE(FloatLiteral)  printf(B, "%gf", it->val); END
   CASE(DoubleLiteral) printf(B, "%g",  it->val); END
   CASE(Label)
      printf(B, "%s:", it->atom->str);
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
   CASE(UnOp)
      if (it->op == OP_POST_INC || it->op == OP_POST_DEC) {
         _print(state, B, it->operand);
         printf(B, "%s", op2str(it->op));
      } else {
         printf(B, "%s", op2str(it->op));
         _print(state, B, it->operand);
      }
   END
   CASE(BinOp)
      _print(state, B, it->left);
      printf(B, " %s ", op2str(it->op), it->op);
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

}

#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
