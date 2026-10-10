#include "lexer.hh"
#include "tokens.hh"

void Lexer::push(TokenID tokid, size_t start, size_t end) {
    tokens_.ids.push_back(tokid);
    tokens_.start_pos.push_back(start);
    tokens_.end_pos.push_back(end);
    size_t jump = end - pos_;
    pos_ = end;
    line_size_ += jump;
}

void Lexer::lex_line_comment() {
    size_t start = pos_;
    advance(2);  // Skip "//"
    while (curr() != '\n') {
        advance(1);
    }
    size_t end = pos_;
    push(LINE_COMMENT, start, end);
    push(ENDL, pos_, pos_ + 1);
    tokens_.line_size.push_back(line_size_);
    line_size_ = 0;
}

void Lexer::lex_identifier() {
    size_t start = pos_;
    advance(1);
    while (is_identifier()) {
        advance(1);
    }
    size_t end = pos_;
    push(IDENT, start, end);
}

void Lexer::lex_int_literal() {
    size_t start = pos_;
    advance(1); // We already know this is a digit
    while (isdigit(curr())) {
        /// FIXME(pauek): Limit the size of this!
        advance(1);
    }
    size_t end = pos_;
    push(INT_LITERAL, start, end);
}

void Lexer::lex_macro() {
    size_t start = pos_;
    advance(1);  // skip '#'
    while (is_identifier()) {
        advance(1);
    }
    size_t end = pos_;
    push(MACRO, start, end);
}

void Lexer::lex_string_literal() {
    size_t start = pos_;
    advance(1);
    while (curr() != '"') {
        advance(1);
    }
    advance(1);  // consume '"'
    size_t end = pos_;
    push(STR_LITERAL, start, end);
}

void Lexer::lex_char_literal() {
    size_t start = pos_;
    advance(1);
    if (curr() == '\\') {
        advance(1); // An escape slash
    }
    advance(1); // The character
    advance(1); // The closing `'`
    size_t end = pos_;
    push(CHAR_LITERAL, start, end);
}

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
        if (is_line_comment()) {
            lex_line_comment();
            continue;
        }
        if (is_macro()) { /// FIXME(pauek): Only at the start of a line!!
            lex_macro();
            continue;
        }
        if (is_multiline_comment()) {
            error("Multi-line comments are not implemented yet!");
        }
        if (is_identifier_start()) {
            lex_identifier();
            continue;
        }
        if (isdigit(curr())) {
            lex_int_literal();
            continue;
        }
        switch (curr()) {
            case '\n': {
                push(ENDL, pos_, pos_ + 1);
                tokens_.line_size.push_back(line_size_);
                line_size_ = 0;
                break;
            }
            case '\'': {
                lex_char_literal();
                break;
            }
            case '"': {
                lex_string_literal();
                break;
            }

            case '-': {
                switch (peek(1)) {
                    case '-': {
                        push(MINUSMINUS, pos_, pos_ + 2);
                        break;
                    }
                    case '=': {
                        push(MINUSEQ, pos_, pos_ + 2);
                        break;
                    }
                    case '>': {
                        push(ARROW, pos_, pos_ + 2);
                        break;
                    }
                    default: {
                        push(MINUS, pos_, pos_ + 1);
                    }
                }
            }

            // One character tokens
#define TOKEN_A(A, ID_A)            \
    case A: {                       \
        push(ID_A, pos_, pos_ + 1); \
        break;                      \
    }
                TOKEN_A('(', OPEN_PAREN)
                TOKEN_A(')', CLOSE_PAREN)

                TOKEN_A('[', OPEN_BRACKET)
                TOKEN_A(']', CLOSE_BRACKET)

                TOKEN_A('{', OPEN_BRACE)
                TOKEN_A('}', CLOSE_BRACE)

                TOKEN_A('?', QUESTION)
                TOKEN_A(':', COLON)
                TOKEN_A(';', SEMICOLON)
                TOKEN_A(',', COMMA)
                TOKEN_A('.', DOT)

                TOKEN_A('!', NOT)
                TOKEN_A('\\', ESC)

#define TOKEN_A_AB(A, B, ID_A, ID_AB)    \
    case A: {                            \
        if (peek(1) == B) {              \
            push(ID_AB, pos_, pos_ + 2); \
        } else {                         \
            push(ID_A, pos_, pos_ + 1);  \
        }                                \
        break;                           \
    }
                TOKEN_A_AB('=', '=', EQ, EQEQ)
                TOKEN_A_AB('*', '=', STAR, STAREQ)
                TOKEN_A_AB('/', '=', DIV, DIVEQ)
                TOKEN_A_AB('%', '=', MOD, MODEQ)
                TOKEN_A_AB('^', '=', XOR, XOREQ)

#define TOKEN_A_AA_AB(A, B, ID_A, ID_AA, ID_AB) \
    case A: {                                   \
        switch (peek(1)) {                      \
            case A: {                           \
                push(ID_AA, pos_, pos_ + 2);    \
                break;                          \
            }                                   \
            case B: {                           \
                push(ID_AB, pos_, pos_ + 2);    \
                break;                          \
            }                                   \
            default: {                          \
                push(ID_A, pos_, pos_ + 1);     \
            }                                   \
        }                                       \
        break;                                  \
    }
    TOKEN_A_AA_AB('+', '=', PLUS, PLUSPLUS, PLUSEQ)
    TOKEN_A_AA_AB('&', '=', AMP, AMPAMP, AMPEQ)
    TOKEN_A_AA_AB('|', '=', BAR, BARBAR, BAREQ)


#define TOKEN_A_AB_AA_AAB(A, B, ID_A, ID_AB, ID_AA, ID_AAB) \
    case A: {                                               \
        switch (peek(1)) {                                  \
            case A: {                                       \
                if (peek(2) == B) {                         \
                    push(ID_AAB, pos_, pos_ + 3);           \
                } else {                                    \
                    push(ID_AA, pos_, pos_ + 2);            \
                }                                           \
                break;                                      \
            }                                               \
            case B: {                                       \
                push(ID_AB, pos_, pos_ + 2);                \
                break;                                      \
            }                                               \
            default: {                                      \
                push(ID_A, pos_, pos_ + 1);                 \
            }                                               \
        }                                                   \
        break;                                              \
    }
                TOKEN_A_AB_AA_AAB('>', '=', GT, GEQ, GTGT, GTGTEQ)
                TOKEN_A_AB_AA_AAB('<', '=', LT, LEQ, LTLT, LTLTEQ)

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
