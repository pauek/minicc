
#include <assert.h>
#include "minicc.hh"

struct LexerState {
	const char *at;
	Pos         pos;
};

static const char *at = 0;
static        Pos  pos = {1, 1};
static const char *lexer_buffer = 0;
static LexerState  lexer_states[LEXER_MAX_STATES] = {};
static        int  lexer_curr_state = -1;

CommentSeq comment_seq;

// Token atoms
#define TOKEN(name, str, len) Atom *tok_##name;
#include "tokens.inc"
#undef TOKEN

void lexer_init_atoms() {
#define TOKEN(name, str, len) tok_##name = atom_get(str, len);
#include "tokens.inc"
#undef TOKEN
}

#define AT(a)     (at[0] == (a))
#define AT2(a, b) (at[0] == (a) && at[1] == (b))
#define AT_SPACE  (at[0] == ' ' || at[0] == '\t' || at[0] == '\f' || at[0] == '\v')
#define AT_END    (at[0] == 0)

void lexer_init(const char *buffer) {
	lexer_buffer = buffer;
	lexer_curr_state = 0;
	at = buffer;
	pos.lin = 1;
	pos.col = 1;
	comment_seq.ncomments = 0;
	lexer_curr_state = -1;
	lexer_init_atoms();
}

void lexer_push() {
	lexer_curr_state++;
	assert(lexer_curr_state < LEXER_MAX_STATES);
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

void lexer_skip_comment(int type) {
	at += 2;
	Comment *c = &comment_seq.comments[comment_seq.ncomments - 1];
	c->type = type;
	c->str = at;
	loop {
		if (AT_END) break;
		if (type == COMMENT_SINGLELINE && AT('\n')) {
			at++;
			break;
		}
		if (type == COMMENT_MULTILINE && AT2('*','/')) {
			at += 2;
			break;
		}
		at++;
	}
	c->len = (size_t)(at - c->str);
	comment_seq.ncomments++;
	assert(comment_seq.ncomments < LEXER_MAX_COMMENTS);
}

static bool lexer_skip_space() {
	const char *start = at;
	comment_seq.ncomments = 0;
	loop {
		if (AT_END) {
			return at > start;
		} else if (AT_SPACE) {
			at++;
		} else if (AT2('/','/')) {
			lexer_skip_comment(COMMENT_SINGLELINE);
		} else if (AT2('/','*')) {
			lexer_skip_comment(COMMENT_MULTILINE);
		} else {
			return at > start;
		}
	}
}

Token lexer_get() {
	bool space = lexer_skip_space();
	if (AT_END) {
		return { 0, pos };
	}
	switch (at[0]) {
	case '+':
		if (at[1] == '+') {
			return { tok_plusplus, pos };
		} else {
			return { tok_plus, pos };
		}
		break;

	default:
		assert(false);
	}
	return { 0, pos };
}