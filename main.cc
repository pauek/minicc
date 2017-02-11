
#include <stdlib.h>
#include <stdio.h>
#include "minicc.hh"

void init() {
	atom_init();
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