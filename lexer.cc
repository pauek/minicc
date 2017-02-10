
#define MAX_STATES

struct LexerState {
	char *at;
	Pos   pos;
};

static       char *lexer_buffer = 0;
static LexerState  lexer_states[MAX_STATES] = {};
static        int  lexer_curr_state = 0;

#define CURR

void lexer_init(const char *buffer) {
	lexer_buffer = buffer;
	lexer_curr_state = 0;
	LexerState *s = lexer_states[lexer_curr_state];
	CURR->at = buffer;
	CURR->pos.lin = 1;
	CURR->pos.col = 1;
}

void lexer_push() {
	int above = lexer_curr_state + 1;
	assert(above < MAX_STATES);
	lexer_states[above] = lexer_states[lexer_curr_state];
	lexer_curr_state = above;
}

void lexer_pop() {
	assert(lexer_curr_state > 0);
	lexer_curr_state--;
}

void lexer_discard() {
	int below = lexer_curr_state-1
	assert(below >= 0);
	lexer_states[below] = lexer_states[lexer_curr_state];
	lexer_curr_state = below;
}

Token lexer_get() {

}