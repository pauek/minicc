
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

int main() {
   atom::init();
   ast::test();
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