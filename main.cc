
#include <stdlib.h>
#include <stdio.h>
#include "minicc.hh"

void init() {
	lexer_init();
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
		if (tok.atom == tok_eof) {
			break;
		}
		printf("%d:%d: %.*s\n", tok.pos.lin, tok.pos.col, (int)tok.atom->len, tok.atom->str);
	}
}