
#if defined(_MSC_VER)
#define WINDOWS
#else
#define LINUX
#endif

#define loop while(1)

const int LEXER_MAX_COMMENTS_BETWEEN_TOKENS = 10;
const int LEXER_MAX_SAVED_STATES = 10;

#define  DECLARATION
#include "minicc.h"
#undef   DECLARATION

#define  IMPLEMENTATION
#include "minicc.h"
#undef   IMPLEMENTATION

void init() {
	atom::init();
}

using namespace ast;

ast::Node *for_i(Node *var, int start, int end, int incr, Node *block) {
   Node *nincr = 0;
   if (incr == 1) {
      nincr = UnOp("_++", var);
   } else {
      nincr = BinOp(var, "+=", Literal(incr));
   }
   return For(BinOp(var, "=", Literal(start)),
              BinOp(var, "<", Literal(end)),
              nincr,
              block);
}


void ast_test() {
   using atom::atom;
   using namespace ast;

   // @Incomplete: We are missing the types!!!!

   Node *a = LocalVar(atom("a"));
   Node *b = LocalVar(atom("b"));
   Node *i = LocalVar(atom("i"));

   Node *B = 
   BlockBegin
      Label(atom("blah")),
      Literal(5),
      Assign(a, Equals(b, Literal(1.4142f))),
      for_i(i, 0, 20, 2,
         BlockBegin
            Literal(3.0),
         BlockEnd
      ),
      If(Equals(a, Literal(7)),
         BlockBegin
            Assign(a, Literal(10)),
            Assign(b, Literal(15)),
         BlockEnd
      ),
   BlockEnd;

   Buffer *buf = buf::make();
   print(buf, B);
   printf("[%d, %d] %s", buf->len, buf->avail, buf->str);
   buf::free(buf);
}

int main() {
   atom::init();
   ast_test();
   // atom::print_all();
}

int main_main(int argc, char *argv[]) {
	init();

	if (argc < 2) {
		printf("usage: minicc <file>\n");
		exit(1);
	}

	const char *buffer = file::read_whole(argv[1]);
	lexer::start(buffer);
	loop {
		lexer::Token tok = lexer::get();
		if (tok.kind == TOK_EOF) {
			break;
		}
		printf("%d:%d: %s, %.*s\n", 
			    tok.pos.lin, tok.pos.col, 
			    lexer::token_kind(tok.kind), 
			    (int)tok.atom->len, tok.atom->str);
	}
   return 0;
}