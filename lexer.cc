
#include <assert.h>
#include <string.h>
#include "minicc.hh"

struct LexerState {
	const char *at;
	Pos         pos;
};

static const char *at = 0;
static        Pos  pos = {1, 1};
static const char *lexer_buffer = 0;
static LexerState  lexer_states[LEXER_MAX_SAVED_STATES] = {};
static        int  lexer_curr_state = -1;
static       bool  at_directive = false;
static       bool  at_include_filename = false;

CommentSeq comment_seq;

#define AT(a)     (at[0] == (a))
#define AT2(a, b) (at[0] == (a) && at[1] == (b))
#define AT_SPACE  (at[0] == ' ' || at[0] == '\t' || at[0] == '\f' || at[0] == '\v')
#define AT_ENDL   (at[0] == '\n') // endls en windows y linux?
#define AT_EOF    (at[0] == 0)

#if defined(DEBUG) 
char *lexer_token_kind(TokenKind kind) {
	static char buffer[32];
	switch (kind) {
	case TOK_EOF:      sprintf(buffer, "EOF"); break;
	case TOK_ERROR:    sprintf(buffer, "ERROR"); break;
	case TOK_OPERATOR: sprintf(buffer, "OPERATOR"); break;
	case TOK_PUNCT:    sprintf(buffer, "PUNCT"); break;
	case TOK_IDENT:    sprintf(buffer, "IDENT"); break;
	case TOK_FILENAME: sprintf(buffer, "FILENAME"); break;
	case TOK_STRING:   sprintf(buffer, "STRING"); break;
	case TOK_INT:      sprintf(buffer, "INT"); break;
	case TOK_FLOAT:    sprintf(buffer, "FLOAT"); break;
	case TOK_CONTROL:  sprintf(buffer, "CONTROL"); break;
	case TOK_BOOL:     sprintf(buffer, "BOOL"); break;
	case TOK_DIRECTIVE:sprintf(buffer, "DIRECTIVE"); break;
	case TOK_TYPE:     sprintf(buffer, "TYPE"); break;
	case TOK_TYPEDEF:  sprintf(buffer, "TYPEDEF"); break;
	case TOK_MODIFIER: sprintf(buffer, "MODIFIER"); break;
	case TOK_USING:    sprintf(buffer, "USING"); break;
	}
	return buffer;
}
#endif

void lexer_start(const char *buffer) {
	lexer_buffer = buffer;
	lexer_curr_state = 0;
	at = buffer;
	pos.lin = 1;
	pos.col = 1;
	comment_seq.ncomments = 0;
	lexer_curr_state = -1;
	at_directive = false;
	at_include_filename = false;
}

void lexer_push() {
	lexer_curr_state++;
	assert(lexer_curr_state < LEXER_MAX_SAVED_STATES);
	lexer_states[lexer_curr_state].at  = at;
	lexer_states[lexer_curr_state].pos = pos;
}

void lexer_pop() {
	assert(lexer_curr_state >= 0);
	at  = lexer_states[lexer_curr_state].at;
	pos = lexer_states[lexer_curr_state].pos;
	lexer_curr_state--;
}

void lexer_discard() {
	assert(lexer_curr_state >= 0);
	lexer_curr_state--;
}

#define ADVANCE(n) { at += (n); pos.col += (n); }

void lexer_skip_comment(int type) {
	ADVANCE(2);
	Comment *c = &comment_seq.comments[comment_seq.ncomments - 1];
	c->type = type;
	c->str = at;
	loop {
		if (AT_EOF) break;
		if (type == COMMENT_SINGLELINE && AT('\n')) {
			ADVANCE(1);
			break;
		}
		if (type == COMMENT_MULTILINE && AT2('*','/')) {
			ADVANCE(2);
			break;
		}
		if (at[0] == '\n') {
			at++;
			pos.lin++;
			pos.col = 1;
		} else {
			ADVANCE(1);
		}
	}
	c->len = (size_t)(at - c->str);
	comment_seq.ncomments++;
	assert(comment_seq.ncomments < LEXER_MAX_COMMENTS_BETWEEN_TOKENS);
}

bool lexer_skip_space() {
	const char *start = at;
	comment_seq.ncomments = 0;
	loop {
		if (AT_EOF) {
			return at > start;
		} else if (AT_SPACE) {
			ADVANCE(1);
		} else if (AT_ENDL) {
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
		if (AT_EOF) {
			break;
		}
		if ((at[0] >= 'a' && at[0] <= 'z') ||
			 (at[0] >= 'A' && at[0] <= 'Z') ||
			 (at[0] >= '0' && at[0] <= '9') ||
			 at[0] == '_') {
			ADVANCE(1);
			continue;
		}
		break;
	}
	Atom *id = atom_get(id_begin, (size_t)(at - id_begin));
	TokenKind kind = TOK_IDENT;

	// maybe set at_include_directive to lex filenames correctly
	

	return { TOK_IDENT, tokpos, id };
}

/*

El lexer devuelve un token, el que encuentre primero, saltando los espacios
pero deja en comment_seq todos los comentarios que se ha encontrado entremedio.
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
	Pos tokpos = pos;
	switch (at[0]) 
	{
	case '(': RESULT(TOK_PUNCT, 1, lparen)
	case ')': RESULT(TOK_PUNCT, 1, rparen)
	case '[': RESULT(TOK_PUNCT, 1, lbracket)
	case ']': RESULT(TOK_PUNCT, 1, rbracket)
	case '{': RESULT(TOK_PUNCT, 1, lbrace)
	case '}': RESULT(TOK_PUNCT, 1, rbrace)

	case ';': RESULT(TOK_PUNCT, 1, semicolon)
	case ':': RESULT(TOK_PUNCT, 1, colon)
	case '?': RESULT(TOK_PUNCT, 1, qmark)
	case ',': RESULT(TOK_PUNCT, 1, comma)
	case '.': RESULT(TOK_PUNCT, 1, dot)

	case '+':
		if (at[1] == '+')      RESULT(TOK_OPERATOR, 2, plusplus)
		else if (at[1] == '=') RESULT(TOK_OPERATOR, 2, pluseq)
		else                   RESULT(TOK_OPERATOR, 1, plus)
		break;

	case '-':
		if (at[1] == '-')      RESULT(TOK_OPERATOR, 2, minusminus)
		else if (at[1] == '=') RESULT(TOK_OPERATOR, 2, minuseq)
		else                   RESULT(TOK_OPERATOR, 1, minus)
		break;

	case '*':
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, stareq)
		else              RESULT(TOK_OPERATOR, 1, star)
		break;

	case '/':
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, slasheq)
		else              RESULT(TOK_OPERATOR, 1, slash)
		break;

	case '^': 
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, xoreq)
		else              RESULT(TOK_OPERATOR, 1, xor)
		break;

	case '|':
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, bareq)
		else              RESULT(TOK_OPERATOR, 1, bar)
		break;

	case '&':
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, ampeq)
		else              RESULT(TOK_OPERATOR, 1, amp)
		break;

	case '%':
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, modeq)
		else              RESULT(TOK_OPERATOR, 1, mod)
		break;

	case '<':
		if (at[1] == '=')  	RESULT(TOK_OPERATOR, 2, leq)
		else if (at[1] == '<') {
			if (at[2] == '=') RESULT(TOK_OPERATOR, 2, lshifteq)
			else              RESULT(TOK_OPERATOR, 2, lshift)
		} 
	   else                 RESULT(TOK_OPERATOR, 1, lt)
		break;

	case '>':
		if (at[1] == '=')  	RESULT(TOK_OPERATOR, 2, geq)
		else if (at[1] == '>') {
			if (at[2] == '=') RESULT(TOK_OPERATOR, 2, rshifteq)
			else              RESULT(TOK_OPERATOR, 2, rshift)
		} 
	   else                 RESULT(TOK_OPERATOR, 1, gt)
		break;

	case '!': 
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, noteq)
		else              RESULT(TOK_OPERATOR, 1, not)
		break;

	case '=':
		if (at[1] == '=') RESULT(TOK_OPERATOR, 2, eqeq)
		else              RESULT(TOK_OPERATOR, 1, eq)
		break;

	case '#': 
		if (pos.col == 1) at_directive = true;
		RESULT(TOK_PUNCT, 1, sharp);

	case 'a':
		IF_ID_RESULT(TOK_MODIFIER, 4, auto)
		IF_ID_RESULT(TOK_OPERATOR, 3, and)
		return lexer_read_identifier();

	case 'b':
		IF_ID_RESULT(TOK_CONTROL, 5, break)
		IF_ID_RESULT(TOK_TYPE, 4, bool)
		return lexer_read_identifier();

	case 'c':
		IF_ID_RESULT(TOK_CONTROL, 8, continue)
		IF_ID_RESULT(TOK_MODIFIER, 5, const)
		IF_ID_RESULT(TOK_TYPEDEF, 5, class)
		IF_ID_RESULT(TOK_TYPE, 4, char)
		IF_ID_RESULT(TOK_CONTROL, 4, case)
		return lexer_read_identifier();

	case 'd':
		IF_ID_RESULT(TOK_TYPE, 6, double)
		return lexer_read_identifier();

	case 'e':
		IF_ID_RESULT(TOK_CONTROL, 4, else)
		IF_ID_RESULT(TOK_TYPEDEF, 4, enum)
		IF_ID_RESULT(TOK_MODIFIER, 6, extern)
		IF_ID_RESULT(TOK_MODIFIER, 8, explicit)
		return lexer_read_identifier();

	case 'f':
		IF_ID_RESULT(TOK_CONTROL, 3, for)
		IF_ID_RESULT(TOK_TYPE, 5, float)
		IF_ID_RESULT(TOK_BOOL, 5, false)
		return lexer_read_identifier();

	case 'g':
		IF_ID_RESULT(TOK_CONTROL, 4, goto)
		return lexer_read_identifier();		

	case 'h':
		return lexer_read_identifier();		

	case 'i':
		IF_ID_RESULT(TOK_CONTROL, 2, if)
		IF_ID_RESULT(TOK_TYPE, 3, int)
		IF_ID_RESULT(TOK_MODIFIER, 6, inline)
		if (!strncmp("include", at, 7)) {
			if (at_directive) {
				at_include_filename = true;
			}
			RESULT(TOK_DIRECTIVE, 7, include)
		}
		return lexer_read_identifier();		

	case 'j':
	case 'k':
		return lexer_read_identifier();		

	case 'l':
		IF_ID_RESULT(TOK_MODIFIER, 4, long)
		return lexer_read_identifier();		

	case 'm':
		IF_ID_RESULT(TOK_MODIFIER, 7, mutable)
		return lexer_read_identifier();		

	case 'n':
		IF_ID_RESULT(TOK_USING, 9, namespace)
		return lexer_read_identifier();		

	case 'o':
		IF_ID_RESULT(TOK_OPERATOR, 2, or)
		return lexer_read_identifier();		

	case 'p':
	case 'q':
		return lexer_read_identifier();		

	case 'r':
		IF_ID_RESULT(TOK_CONTROL, 6, return)
		IF_ID_RESULT(TOK_MODIFIER, 8, register)
		return lexer_read_identifier();		

	case 's':
		IF_ID_RESULT(TOK_TYPE, 5, short)
		IF_ID_RESULT(TOK_TYPE, 6, string)
		IF_ID_RESULT(TOK_CONTROL, 6, switch)
		IF_ID_RESULT(TOK_MODIFIER, 6, static)
		IF_ID_RESULT(TOK_TYPEDEF, 6, struct)
		return lexer_read_identifier();		

	case 't':
		IF_ID_RESULT(TOK_CONTROL, 4, true)
		IF_ID_RESULT(TOK_CONTROL, 7, typedef)
		return lexer_read_identifier();		

	case 'u':
		IF_ID_RESULT(TOK_CONTROL, 4, unsigned)
		IF_ID_RESULT(TOK_USING, 7, using)
		return lexer_read_identifier();		

	case 'v':
		IF_ID_RESULT(TOK_TYPE, 4, void)
		IF_ID_RESULT(TOK_MODIFIER, 8, volatile)
		IF_ID_RESULT(TOK_MODIFIER, 7, virtual)
		return lexer_read_identifier();		

	case 'w':
		IF_ID_RESULT(TOK_CONTROL, 5, while)
		return lexer_read_identifier();		

	case 'x':
	case 'y':
	case 'z':
		return lexer_read_identifier();

	default:
		if ((at[0] >= 'A' && at[0] <= 'Z') || at[0] == '_') {
			return lexer_read_identifier();
		}
		RESULT(TOK_ERROR, 1, error);
	}
}