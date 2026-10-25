#include <iostream>

#include "tokens.hh"
using namespace std;

#include "hl-lexer.hh"

void HLLexer::dump(std::ostream& out) {
    out << lexer_.tokens().ids.size() << endl;
}

void HLLexer::parse_brackets() {
	assert(at(OPEN_BRACKET));
	advance(1);

	size_t start = pos_;
	while (not at(CLOSE_BRACKET)) {
		advance(1);
	}
	advance(1); // consume closing bracket
	size_t end = pos_;
	tokens().hl_push(BRACKETS, start, end);
}

void HLLexer::parse_block() {
	assert(at(OPEN_BRACE));
	advance(1);

	size_t start = pos_;
	while (not at(CLOSE_BRACE)) {
		advance(1);
	}
	size_t end = pos_;
	tokens().hl_push(BLOCK, start, end);
}

void HLLexer::parse_parentheses() {
	assert(at(OPEN_PAREN));
}

void HLLexer::parse_token_group() {
	size_t start = pos_;
	while (not at_punct()) {
		advance(1);
	}
	size_t end = pos_;
	tokens().hl_push(TOKEN_SEQ, start, end);
}

void HLLexer::run() {
    Tokens& tokens = lexer_.tokens();
    while (not finished()) {
        TokenID tok = tokens.ids[pos_];
        switch (tok) {
            case OPEN_BRACKET: {
            	parse_brackets();
            	break;
            }
            case OPEN_PAREN: {
            	parse_parentheses();
                break;
            }
            case OPEN_BRACE: {
            	parse_block();
                break;
            }
            default: {
                parse_token_group();
            }
        }
    }
}
