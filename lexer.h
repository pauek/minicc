/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)


#include <assert.h>
#include <stddef.h>


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


#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)

struct LexerState {
	const char *at;
	Pos         pos;
};

static const char *at = 0;
static        Pos  pos = {1, 1};
static const char *buffer = 0;
static LexerState  states[LEXER_MAX_SAVED_STATES] = {};
static        int  top = -1;
static       bool  at_directive = false;
static       bool  at_include_filename = false;
       CommentSeq  lexer_comment_seq;

#define AT(a)     (at[0] == (a))
#define AT2(a, b) (at[0] == (a) && at[1] == (b))
#define AT_EOF    (at[0] == 0)
#define ENDL(c)   ((c) == '\n') // endls en windows y linux?
#define SPACE(c)  ((c) == ' ' || (c) == '\t' || (c) == '\f' || (c) == '\v')
#define DIGIT(c)  ((c) >= '0' && (c) <= '9')
#define LOWER(c)  ((c) >= 'a' && (c) <= 'z')
#define UPPER(c)  ((c) >= 'A' && (c) <= 'Z')

void lexer_start(const char *buf) {
	buffer = buf;
	top = -1;
	pos = { 1, 1 };
	at = buffer;
	lexer_comment_seq.ncomments = 0;
	at_directive = false;
	at_include_filename = false;
}

void lexer_push() {
	top++;
	assert(top >= 0 && top < LEXER_MAX_SAVED_STATES);
	states[top].at  = at;
	states[top].pos = pos;
}

void lexer_pop() {
	assert(top >= 0 && top < LEXER_MAX_SAVED_STATES);
	at  = states[top].at;
	pos = states[top].pos;
	top--;
}

void lexer_discard() {
	assert(top >= 0);
	top--;
}

#define ADVANCE(n) { at += (n); pos.col += (n); }

void lexer_skip_comment(int type) {
	ADVANCE(2);
	Comment *c = &lexer_comment_seq.comments[lexer_comment_seq.ncomments - 1];
	c->type = type;
	c->str = at;
	loop {
		if (AT_EOF) break;
		if (AT('\n')) {
			at++;
			pos.lin++;
			pos.col = 1;
			if (type == COMMENT_SINGLELINE) {
				break;
			}
		}
		if (type == COMMENT_MULTILINE && AT2('*','/')) {
			ADVANCE(2);
			break;
		}
		ADVANCE(1);
	}
	c->len = (size_t)(at - c->str);
	lexer_comment_seq.ncomments++;
	assert(lexer_comment_seq.ncomments < LEXER_MAX_COMMENTS_BETWEEN_TOKENS);
}

bool lexer_skip_space() {
	const char *start = at;
	lexer_comment_seq.ncomments = 0;
	loop {
		if (AT_EOF) {
			return at > start;
		} else if (SPACE(at[0])) {
			ADVANCE(1);
		} else if (ENDL(at[0])) {
			at++;
			pos.lin++;
			pos.col = 1;
		} else if (AT2('/','/')) {
			lexer_skip_comment(COMMENT_SINGLELINE);
		} else if (AT2('/','*')) {
			lexer_skip_comment(COMMENT_MULTILINE);
		} else {
			return at > start;
		}
	}
}

static Token lexer_read_include_filename() {
	// skip until non-space
	loop {
		if (at[0] == 0) {
			return { TOK_EOF, pos, NULL };
		}
		if (at[0] == ' ' || at[0] == '\t') {
			ADVANCE(1);
			continue;
		}
		break;
	}

	// find out delimiter
	char delimiter;
	if (at[0] == '<') {
		delimiter = '>';
	} else if (at[0] == '"') {
		delimiter = '"';
	} else {
		return { TOK_ERROR, pos, NULL };
	}
	ADVANCE(1);
	const char *filename_begin = at;
	Pos tokpos = pos;

	// look until next delimiter
	loop {
		if (at[0] == 0) {
			return { TOK_EOF, pos, NULL };
		}
		if (at[0] != delimiter) {
			ADVANCE(1);
			continue;
		}
		break;
	}
	assert(at[0] == delimiter);

	size_t filename_size = (size_t)(at - filename_begin);
	ADVANCE(1);
	Atom *a = atom_get(filename_begin, filename_size);
	return { TOK_FILENAME, tokpos, a };
}

static Token lexer_read_identifier() {
	const char *id_begin = at;
	Pos tokpos = pos;
	ADVANCE(1);
	loop {
		if (AT_EOF) break;
		if (LOWER(at[0]) || UPPER(at[0]) || DIGIT(at[0]) || at[0] == '_') {
			ADVANCE(1);
			continue;
		}
		break;
	}
	Atom *id = atom_get(id_begin, (size_t)(at - id_begin));
	TokenKind kind = TOK_IDENT;
	return { TOK_IDENT, tokpos, id };
}

static Token lexer_read_number() {
	const char *id_begin = at;
	const char *id_end;
	Pos tokpos = pos;
	bool real_number = false;
	if (at[0] == '-') {
		ADVANCE(1);
	}
	loop {
		if (AT_EOF) break;
		else if (DIGIT(at[0])) {
			ADVANCE(1);
			continue;
		} else if (at[0] == '.') {
			if (!real_number) {
				real_number = true;
				ADVANCE(1);
				continue;
			} else {
				break;
			}
		}
		break;
	}
	id_end = at;                 // do not put 'f' into the atom
	TokenKind kind = TOK_LIT_INT;
	if (real_number) {
		kind = TOK_LIT_DOUBLE;
		if (at[0] == 'f') { 
			ADVANCE(1);
			kind = TOK_LIT_FLOAT;
		}
	}
	Atom *atom = atom_get(id_begin, (size_t)(id_end - id_begin));
	return { kind, tokpos, atom };
}

Token lexer_read_literal_char_or_string() {
	char delimiter = at[0];
	TokenKind kind = (delimiter == '\'' ? TOK_LIT_CHAR : TOK_LIT_STRING);
	ADVANCE(1);
	bool slash_error = false;
	int nchars = 0;
	const char *tok_begin = at;
	Pos tokpos = pos;
	loop {
		if (at[0] == delimiter) {
			break;
		}
		if (ENDL(at[0])) {
			return { TOK_ERROR, pos, NULL };
		}
		if (at[0] == '\\') {
			ADVANCE(1);
			switch (at[0]) {
			case 'a': case 'b': case 'f': case 'n': case 'r':
			case 't': case 'v': case '\'': case '\"': 
			case '\?': case '\\': 
				break;
         default:
         	slash_error = true; // how to handle this??
			}
		}
		ADVANCE(1);
		nchars++;
	}
	size_t len = (size_t)(at - tok_begin);
	ADVANCE(1); // consume delimiter
	if (slash_error) {
		return { TOK_ERROR, tokpos, NULL };
	} else {
		Atom *atom = atom_get(tok_begin, len);
		return { kind, tokpos, atom };
	}
}

/*

El lexer devuelve un token, el que encuentre primero, saltando los espacios
pero deja en lexer_comment_seq todos los comentarios que se ha encontrado entremedio.
La secuencia de comentarios es una variable global y se resetea cada vez.
Esto permite luego recuperarlos (si es necesario), porque si quieres imprimir
el código original desde el AST debes respetar los comentarios del usuario y deben, 
por tanto, estar guardados en el AST.
 
   -pauek, 12 Febrero 2017

*/

#define RESULT(kind, n, token) \
   { ADVANCE(n); return { kind, tokpos, atom_##token }; }

#define IF_ID_RESULT(kind, n, token) \
   if (!strncmp(#token, at, n)) { RESULT(kind, n, token); }

Token lexer_get() {
	bool space = lexer_skip_space();
	if (AT_EOF) {
		return { TOK_EOF, pos, NULL };
	}
	// filenames are parsed specially in 'include' directives.
	if (at_include_filename) {
		at_include_filename = false;
		at_directive = false;
		return lexer_read_include_filename();
	}
	Token tok;
	Pos tokpos = pos;
	switch (at[0]) 
	{
	case '(': RESULT(TOK_DELIM, 1, lparen)
	case ')': RESULT(TOK_DELIM, 1, rparen)
	case '[': RESULT(TOK_DELIM, 1, lbracket)
	case ']': RESULT(TOK_DELIM, 1, rbracket)
	case '{': RESULT(TOK_DELIM, 1, lbrace)
	case '}': RESULT(TOK_DELIM, 1, rbrace)

	case ';': RESULT(TOK_PUNCT, 1, semicolon)
	case '?': RESULT(TOK_PUNCT, 1, qmark)
	case ',': RESULT(TOK_PUNCT, 1, comma)

	case '.': 
		if (DIGIT(at[1]))
			return lexer_read_number();
		else
			RESULT(TOK_PUNCT, 1, dot)

	case ':': 
		if (at[1] == ':')      RESULT(TOK_PUNCT, 2, coloncolon)
		else                   RESULT(TOK_PUNCT, 1, colon)

	case '+':
		if (at[1] == '+')      RESULT(TOK_OPERATOR, 2, plusplus)
		else if (at[1] == '=') RESULT(TOK_OPERATOR, 2, pluseq)
		else                   RESULT(TOK_OPERATOR, 1, plus)

	case '-':
		if (at[1] == '-')
			RESULT(TOK_OPERATOR, 2, minusminus)
		else if (at[1] == '=')
			RESULT(TOK_OPERATOR, 2, minuseq)
		else if (at[1] == '>')
			RESULT(TOK_OPERATOR, 2, arrow)
		else if (at[1] == '>')
			RESULT(TOK_OPERATOR, 2, arrow)
		else if (DIGIT(at[1]))
			return lexer_read_number();
		else
			RESULT(TOK_OPERATOR, 1, minus)

	case '*':
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, stareq)
		else              RESULT(TOK_OPERATOR, 1, star)

	case '/':
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, slasheq)
		else              RESULT(TOK_OPERATOR, 1, slash)

	case '^': 
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, xoreq)
		else              RESULT(TOK_OPERATOR, 1, xor)

	case '|':
		if (at[1] == '|')      RESULT(TOK_OPERATOR, 2, barbar)
		else if (at[1] == '=') RESULT(TOK_OPERATOR, 2, bareq)
		else                   RESULT(TOK_OPERATOR, 1, bar)

	case '&':
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, ampeq)
		else              RESULT(TOK_OPERATOR, 1, amp)

	case '%':
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, modeq)
		else              RESULT(TOK_OPERATOR, 1, mod)

	case '<':
		if (at[1] == '=')  	RESULT(TOK_OPERATOR, 2, leq)
		else if (at[1] == '<') {
			if (at[2] == '=') RESULT(TOK_OPERATOR, 2, lshifteq)
			else              RESULT(TOK_OPERATOR, 2, lshift)
		} 
	   else                 RESULT(TOK_OPERATOR, 1, lt)

	case '>':
		if (at[1] == '=')  	RESULT(TOK_OPERATOR, 2, geq)
		else if (at[1] == '>') {
			if (at[2] == '=') RESULT(TOK_OPERATOR, 2, rshifteq)
			else              RESULT(TOK_OPERATOR, 2, rshift)
		} 
	   else                 RESULT(TOK_OPERATOR, 1, gt)

	case '!': 
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, noteq)
		else              RESULT(TOK_OPERATOR, 1, not)

	case '=':
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, eqeq)
		else              RESULT(TOK_OPERATOR, 1, eq)

	case '#': 
		if (pos.col == 1) at_directive = true;
		RESULT(TOK_PUNCT, 1, sharp);

	case '0': case '1': case '2': case '3':
	case '4': case '5': case '6': case '7':
	case '8': case '9':
		return lexer_read_number();

	case 'a':
		tok = lexer_read_identifier();
		if      (tok.atom == atom_auto) tok.kind = TOK_MODIFIER;
		else if (tok.atom == atom_and)  tok.kind = TOK_OPERATOR;
		return tok;

	case 'b':
		tok = lexer_read_identifier();
		if      (tok.atom == atom_break) tok.kind = TOK_CONTROL;
		else if (tok.atom == atom_bool)  tok.kind = TOK_TYPE;
		return tok;

	case 'c':
		tok = lexer_read_identifier();
		if      (tok.atom == atom_continue) tok.kind = TOK_CONTROL;
		else if (tok.atom == atom_const)    tok.kind = TOK_MODIFIER;
		else if (tok.atom == atom_class)    tok.kind = TOK_TYPEDEF;
		else if (tok.atom == atom_char)     tok.kind = TOK_TYPE;
		else if (tok.atom == atom_case)     tok.kind = TOK_CONTROL;
		return tok;

	case 'd':
		tok = lexer_read_identifier();
		if (tok.atom == atom_double) tok.kind = TOK_TYPE;
		return tok;		

	case 'e':
		tok = lexer_read_identifier();
		if      (tok.atom == atom_else)     tok.kind = TOK_CONTROL;
		else if (tok.atom == atom_enum)     tok.kind = TOK_TYPEDEF;
		else if (tok.atom == atom_extern)   tok.kind = TOK_MODIFIER;
		else if (tok.atom == atom_explicit) tok.kind = TOK_MODIFIER;
		return tok;

	case 'f':
		tok = lexer_read_identifier();
		if      (tok.atom == atom_for)   tok.kind = TOK_CONTROL;
		else if (tok.atom == atom_float) tok.kind = TOK_TYPE;
		else if (tok.atom == atom_false) tok.kind = TOK_LIT_BOOL;
		return tok;

	case 'g':
		tok = lexer_read_identifier();
		if (tok.atom == atom_goto) tok.kind = TOK_CONTROL;
		return tok;		

	case 'h':
		return lexer_read_identifier();		

	case 'i':
		tok = lexer_read_identifier();
		if      (tok.atom == atom_if)     tok.kind = TOK_CONTROL;
		else if (tok.atom == atom_int)    tok.kind = TOK_TYPE;
		else if (tok.atom == atom_inline) tok.kind = TOK_MODIFIER;
		else if (tok.atom == atom_include) {
			if (at_directive) {
				at_include_filename = true;
			}
			tok.kind = TOK_DIRECTIVE;
		}
		return tok;

	case 'j': case 'k':
		return lexer_read_identifier();		

	case 'l':
		tok = lexer_read_identifier();
		if (tok.atom == atom_long) tok.kind = TOK_MODIFIER;
		return tok;		

	case 'm':
		tok = lexer_read_identifier();
		if (tok.atom == atom_mutable) tok.kind = TOK_MODIFIER;
		return tok;		

	case 'n':
		tok = lexer_read_identifier();
		if (tok.atom == atom_using) tok.kind = TOK_USING;
		return tok;		

	case 'o':
		tok = lexer_read_identifier();
		if (tok.atom == atom_or) tok.kind = TOK_OPERATOR;
		return tok;		

	case 'p': case 'q':
		return lexer_read_identifier();		

	case 'r':
		tok = lexer_read_identifier();
		if      (tok.atom == atom_return)   tok.kind = TOK_CONTROL;
		else if (tok.atom == atom_register) tok.kind = TOK_MODIFIER;
		return tok;

	case 's':
		tok = lexer_read_identifier();
		if      (tok.atom == atom_short)   tok.kind = TOK_CONTROL;
		else if (tok.atom == atom_string)  tok.kind = TOK_TYPE;
		else if (tok.atom == atom_switch)  tok.kind = TOK_CONTROL;
		else if (tok.atom == atom_static)  tok.kind = TOK_MODIFIER;
		else if (tok.atom == atom_struct)  tok.kind = TOK_TYPEDEF;
		return tok;

	case 't':
		tok = lexer_read_identifier();
		if      (tok.atom == atom_true)    tok.kind = TOK_LIT_BOOL;
		else if (tok.atom == atom_typedef) tok.kind = TOK_TYPEDEF;
		return tok;

	case 'u':
		tok = lexer_read_identifier();
		if      (tok.atom == atom_unsigned) tok.kind = TOK_MODIFIER;
		else if (tok.atom == atom_using)    tok.kind = TOK_USING;
		return tok;

	case 'v':
		tok = lexer_read_identifier();
		if      (tok.atom == atom_void)     tok.kind = TOK_TYPE;
		else if (tok.atom == atom_volatile) tok.kind = TOK_MODIFIER;
		else if (tok.atom == atom_virtual)  tok.kind = TOK_MODIFIER;
		return tok;

	case 'w':
		tok = lexer_read_identifier();
		if (tok.atom == atom_while) tok.kind = TOK_CONTROL;
		return tok;

	case 'x': case 'y': case 'z':
		return lexer_read_identifier();

	case '\'': case '"':
		return lexer_read_literal_char_or_string();

	case '\\':
		// Slash at the end of a line is meant for macros.
		RESULT(TOK_BACKSLASH, 1, backslash)

	default:
		if (UPPER(at[0]) || at[0] == '_') {
			return lexer_read_identifier();
      }
      RESULT(TOK_ERROR, 1, error);
	}
}

Token lexer_peek() {
	const char *_at  = at;
	Pos   _pos = pos;	
	Token tok = lexer_get();
	at  = _at;
	pos = _pos;
	return tok;
}


#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
