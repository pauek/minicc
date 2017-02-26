
#if defined(_MSC_VER)
#define WINDOWS
#else
#define LINUX
#endif

#define LEXER_MAX_COMMENTS_BETWEEN_TOKENS 10
#define LEXER_MAX_SAVED_STATES 10
#define loop while(1)

#define  DECLARATION
#include "minicc.h"
#undef   DECLARATION

#define  IMPLEMENTATION
#include "minicc.h"
#undef   IMPLEMENTATION

void init() {
	atom_init();
}

int main_test() {
   ast_test();
}

int main(int argc, char *argv[]) {
	init();

	if (argc < 2) {
		printf("usage: minicc <file>\n");
		exit(1);
	}

	const char *buffer = read_whole_file(argv[1]);
	lexer_start(buffer);
	loop {
		Token tok = lexer_get();
		if (tok.kind == TOK_EOF) {
			break;
		}
		printf("%d:%d: %s, %.*s\n", 
			    tok.pos.lin, tok.pos.col, 
			    lexer_token_kind(tok.kind), 
			    (int)tok.atom->len, tok.atom->str);
	}
}