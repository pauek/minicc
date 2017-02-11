
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

// Token atoms
#define TOKEN(idx, name, str, len) Atom *tok_##name;
#include "tokens.inc"
#undef TOKEN

// Registra todos los atoms para cada tipo de token
void lexer_init_atoms() {
#define TOKEN(idx, name, str, len) tok_##name = atom_get(str, len);
#include "tokens.inc"
#undef TOKEN
}

#define AT(a)     (at[0] == (a))
#define AT2(a, b) (at[0] == (a) && at[1] == (b))
#define AT_SPACE  (at[0] == ' ' || at[0] == '\t' || at[0] == '\f' || at[0] == '\v')
#define AT_END    (at[0] == 0)

void lexer_init() {
	lexer_init_atoms();
}

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
	lexer_init_atoms();
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
		if (AT_END) break;
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
		if (AT_END) {
			return at > start;
		} else if (AT_SPACE) {
			if (at[0] == '\n') {
				at++;
				pos.lin++;
				pos.col = 1;
			} else {
				ADVANCE(1);
			}
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
			return { 0, pos, tok_eof };
		}
		if (at[0] == ' ' || at[0] == '\t') {
			ADVANCE(1);
			continue;
		}
		break;
	}

	// find out delimiter
	char delimiter;
	const char *filename_begin, *filename_end;
	switch (at[0]) {
	case '<':
		ADVANCE(1);
		delimiter = '>';
		filename_begin = at;
		break;

	case '"':
		delimiter = '"';
		filename_begin = at;
		break;

	default:
		return { 0, pos, tok_error };
	}

	// look until next delimiter
	loop {
		if (at[0] == 0) {
			return { 0, pos, tok_eof };
		}
		if (at[0] != delimiter) {
			ADVANCE(1);
			continue;
		}
		break;
	}
	assert(at[0] == delimiter);

	size_t filename_size = at - filename_begin;
	ADVANCE(1);
	Atom *a = atom_get(filename_begin, filename_size);
	return { 0, pos, a };
}

static Token lexer_read_identifier() {
	const char *id_begin = at;
	ADVANCE(1);
	loop {
		if (AT_END) {
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
	Atom *id = atom_get(id_begin, at - id_begin);
	if (id == tok_include) {
		if (at_directive) {
			at_include_filename = true;
		}
	}
	return { 0, pos, id };
}

/*

El lexer devuelve un token, el que encuentre primero, saltando los espacios
pero deja en comment_seq todos los comentarios que se ha encontrado entremedio.
La secuencia de comentarios es una variable global y se resetea cada vez.
Esto permite luego recuperarlos (si es necesario), porque si quieres hacer formateo
de código debes respetar los comentarios del usuario y deben, por tanto, estar 
guardados en el AST.
 
   -pauek, 12 Febrero 2017

*/

#define RESULT(n, token) { ADVANCE(n); return { 0, tokpos, tok_##token }; }
#define IF_KEYWORD_RESULT(n, keyword) if (!strncmp(#keyword, at, (n))) RESULT(n, keyword);

Token lexer_get() {
	bool space = lexer_skip_space();
	if (AT_END) {
		return { 0, pos, tok_eof };
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
	case '+':
		if (at[1] == '+')      RESULT(2, plusplus)
		else if (at[1] == '=') RESULT(2, pluseq)
		else                   RESULT(1, plus)
		break;

	case '-':
		if (at[1] == '-')      RESULT(2, minusminus)
		else if (at[1] == '=') RESULT(2, minuseq)
		else                   RESULT(1, minus)
		break;

	case '*':
		if (at[1] == '=') RESULT(2, stareq)
		else              RESULT(1, star)
		break;

	case '/':
		if (at[1] == '=') RESULT(2, slasheq)
		else              RESULT(1, slash)
		break;

	case '%':
		if (at[1] == '=') RESULT(2, modeq)
		else              RESULT(1, mod)
		break;

	case '<':
		if (at[1] == '=')      RESULT(2, leq)
		else if (at[1] == '<') RESULT(2, lshift)
		else                   RESULT(1, lt)
		break;

	case '>':
		if (at[1] == '=')      RESULT(2, geq)
		else if (at[1] == '>') RESULT(2, rshift)
		else                   RESULT(1, gt)
		break;

	case '(': RESULT(1, lparen);
	case ')': RESULT(1, rparen);
	case '[': RESULT(1, lbracket);
	case ']': RESULT(1, rbracket);
	case '{': RESULT(1, lbrace);
	case '}': RESULT(1, rbrace);

	case '#': 
		if (pos.col == 1) at_directive = true;
		RESULT(1, sharp);

	case 'a' ... 'z':
	case 'A' ... 'Z':
	case '_': 
		return lexer_read_identifier();
	}
	RESULT(1, error);
}