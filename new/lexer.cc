#include "lexer.hh"

// Returns `true` if the lexer is not finished yet
void Lexer::next() {
	while (true) {
		if (finished()) {
			break;
		}
		if (is_space()) {
			advance(1);
			continue;
		}
		if (is_comment()) {
			skip_line_comment();
			continue;
		}
		if (is_identifier_start()) {
			lex_identifier();
			continue;
		}
		switch (curr()) {
			case '*': {
				lex_identifier();
				break;
			}
			case '"': {
				lex_string_literal();
				break;
			}
			case '\n': {
				push(ENDL, pos_, pos_ + 1);
				tokens_.line_size.push_back(line_size_);
				line_size_ = 0;
				break;
			}

			// One character tokens
#define ONECHAR_TOKEN(ch, id)     \
	case ch: {                    \
		push(id, pos_, pos_ + 1); \
		break;                    \
	}
				ONECHAR_TOKEN('=', ASSIGN)
				ONECHAR_TOKEN('?', QUESTION)
				ONECHAR_TOKEN('(', OPEN_PAREN)
				ONECHAR_TOKEN(')', CLOSE_PAREN)
				ONECHAR_TOKEN('[', OPEN_BRACKET)
				ONECHAR_TOKEN(']', CLOSE_BRACKET)
				ONECHAR_TOKEN('{', OPEN_BRACE)
				ONECHAR_TOKEN('}', CLOSE_BRACE)
				ONECHAR_TOKEN(':', COLON)
				ONECHAR_TOKEN(';', SEMICOLON)
				ONECHAR_TOKEN(',', COMMA)

				ONECHAR_TOKEN('<', LT)
				ONECHAR_TOKEN('|', BAR)

			case '>': {
				if (at(1) == '>') {
					push(GTGT, pos_, pos_ + 2);
				} else {
					push(GT, pos_, pos_ + 1);
				}
				break;
			}
			case '-': {
				if (at(1) == '>') {
					push(ARROW, pos_, pos_ + 2);
				} else {
					error("Unexpected '-'");
				}
				break;
			}
			default: {
				char c = curr();
				error("Unexpected character '{}'", c);
			}
		}
	}
}

Tokens Lexer::run() {
	next();
	while (not finished()) {
		next();
	}
	return tokens_;
}
