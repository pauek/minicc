
#include <stdint.h>
#include <stddef.h>

#define LEXER_MAX_COMMENTS_BETWEEN_TOKENS 10
#define LEXER_MAX_SAVED_STATES 10

#define loop while(1)

struct Pos { int lin, col; };

struct Atom {
	const char *str;
	size_t      len;
};

struct Token {
	int   kind;
	Pos   pos;
	Atom *atom;
};

enum {
	COMMENT_MULTILINE,
	COMMENT_SINGLELINE
};

struct Comment {
	int         type;
	const char *str;
	size_t      len;
};

struct CommentSeq {
	Comment comments[LEXER_MAX_COMMENTS_BETWEEN_TOKENS];
	int    ncomments;
};

// tokens

enum {
	TOK_COMMENT,
	TOK_IDENT,
	TOK_KEYWORD,
	TOK_OPERATOR,
};

// atom
Atom *atom_get(const char *str, size_t len);
void print_all_atoms();

// file
char *read_whole_file(const char *filename);

// lexer
#define TOKEN(idx, name, str, len) extern Atom *tok_##name;
#include "tokens.inc"
#undef TOKEN

extern CommentSeq comment_seq;
void  lexer_init();
void  lexer_start(const char *buffer);
void  lexer_push();
void  lexer_pop();
void  lexer_discard();
bool  lexer_skip_space();  // Devuelve 'true' si ha encontrado espacios. Deja comentarios en 'comment_seq'
Token lexer_get();         // Llama a 'lexer_skip_space' antes.

