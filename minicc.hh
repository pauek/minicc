
#include <stdint.h>

struct Pos { int lin, col; };

struct Token {
	Pos pos;
	int len;
};

struct Atom {
	const char *str;
	size_t      len;
};

Atom *atom_get(const char *str, size_t len);

void  lexer_init(const char *buffer);
void  lexer_push();
void  lexer_pop();
void  lexer_discard();
Token lexer_get();

