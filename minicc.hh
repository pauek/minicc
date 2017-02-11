
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
	Atom *atom;
	Pos   pos;
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

// lexer
extern CommentSeq comment_seq;
void  lexer_init(const char *buffer);
void  lexer_push();
void  lexer_pop();
void  lexer_discard();
Token lexer_get();

