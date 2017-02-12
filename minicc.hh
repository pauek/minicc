
#if defined(_MSC_VER)
#define WINDOWS
#include <windows.h>
#else
#define LINUX
#include <unistd.h>
#include <sys/mman.h>
#endif

#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

#define LEXER_MAX_COMMENTS_BETWEEN_TOKENS 10
#define LEXER_MAX_SAVED_STATES 10

#define loop while(1)

struct Pos { int lin, col; };

struct Atom {
	const char *str;
	size_t      len;
};

enum TokenKind {
	TOK_EOF,
	TOK_ERROR,

	TOK_BACKSLASH,
	TOK_DIRECTIVE,
	TOK_FILENAME,
	TOK_USING,

	TOK_PUNCT,
	TOK_DELIM,

	TOK_OPERATOR,

	TOK_IDENT,
	TOK_CONTROL,
	TOK_TYPEDEF,

	TOK_MODIFIER,
	TOK_TYPE,

	TOK_LIT_BOOL,
	TOK_LIT_INT,
	TOK_LIT_FLOAT,
	TOK_LIT_DOUBLE,
	TOK_LIT_CHAR,
	TOK_LIT_STRING,
};

struct Token {
	TokenKind kind;
	Pos       pos;
	Atom     *atom;
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

// atom
#define TOKEN(name, str, len) extern Atom *atom_##name;
#include "tokens.inc"
#undef TOKEN

void atom_init();
Atom *atom_get(const char *str, size_t len);
void print_all_atoms();

// file
char *read_whole_file(const char *filename);

// lexer
extern CommentSeq comment_seq;
#if defined(DEBUG)
char *lexer_token_kind(TokenKind kind);
#endif
void  lexer_start(const char *buffer);
void  lexer_push();
void  lexer_pop();
void  lexer_discard();
bool  lexer_skip_space();  // Devuelve 'true' si ha encontrado espacios. Deja comentarios en 'comment_seq'
Token lexer_get();         // Llama a 'lexer_skip_space' antes.

