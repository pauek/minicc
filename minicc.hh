
#if defined(_MSC_VER)
#define WINDOWS
#else
#define LINUX
#endif

#include <assert.h>
#include <stdint.h>

#define LEXER_MAX_COMMENTS_BETWEEN_TOKENS 10
#define LEXER_MAX_SAVED_STATES 10

#define loop while(1)

//
// atom
//
struct Atom {
	const char *str;
	size_t      len;
};

#define TOKEN(name, str, len) extern Atom *atom_##name;
#include "tokens.inc"
#undef TOKEN

             void  atom_init();
             Atom *atom_get(const char *str, size_t len);

//
// array
//
struct Array {
   size_t   len;
   size_t   size;
   uint8_t *data;
};

            Array *array_new(size_t len, size_t size);
             void  array_free(Array *array);
             void *array_get(Array *array, int i);
             void *array_put(Array *array, int i, void *elem);
             void  array_resize(Array *array, size_t len);
            Array *array_copy(Array *array, size_t len);
    inline size_t  array_len(Array *array)  { assert(array); return array->len; }
    inline size_t  array_size(Array *array) { assert(array); return array->size; }

//
// file
//
             char *read_whole_file(const char *filename);

//
// lexer
//
struct Pos { 
	int lin;
	int col;
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

extern CommentSeq  lexer_comment_seq;
             void  lexer_start(const char *buffer);
             void  lexer_push();
             void  lexer_pop();
             void  lexer_discard();
             bool  lexer_skip_space();  // Devuelve 'true' si ha encontrado espacios. Deja comentarios en 'comment_seq'
            Token  lexer_get();         // Llama a 'lexer_skip_space' antes.
            Token  lexer_peek();        // Devuelve el próximo token, sin avanzar.

// 
// debug
//
#if defined(DEBUG)
             char *lexer_token_kind(TokenKind kind);
             void  print_all_atoms();
#endif

