#include "lexer.hh"

#include <algorithm>
#include <cassert>

#include "tokens.hh"
using namespace std;

void Lexer::push(TokenID tokid, size_t start, size_t end) {
    //
    // FIXME(pauek): This is not good... we should decouple the advance(n)
    // from push(...), otherwise you cannot make tokens that span less than the
    // positions you have walked on. For string/char literals, filenames, etc.
    // that is not convenient.
    //
    tokens_.push(tokid, start, end);
    size_t jump = end - pos_;
    pos_ = end;
    line_size_ += jump;
}

void Lexer::lex_line_comment() {
    size_t start = pos_;
    advance(2);  // Skip "//"
    while (not at('\n')) {
        advance(1);
    }
    size_t end = pos_;
    push(LINE_COMMENT, start, end);
    push(ENDL, pos_, pos_ + 1);
    new_line();
}

void Lexer::lex_multiline_comment() {
    assert(at('/', '*'));
    size_t start = pos_;
    advance(2);
    while (not at('*', '/')) {
        // NOTE(pauek): Take into account that ENDL in Windows is \r\n,
        // and in Unix is \n, and old Mac was \r.
        if (at('\r', '\n')) {
            advance(2);
            new_line();
        } else if (at('\r') || at('\n')) {
            advance(1);
            new_line();
        } else {
            advance(1);
        }
    }
    advance(2);
    size_t end = pos_;
    push(MULTILINE_COMMENT, start, end);
}

void Lexer::lex_endl() {
    // NOTE(pauek): Take into account that ENDL in Windows is "\r\n",
    // in Unix is '\n', and in old Mac was '\r'.
    assert(at('\n') || at('\r'));
    size_t start = pos_;
    size_t end = pos_ + 1;
    if (at('\r', '\n')) {
        end = pos_ + 2;
    }
    push(ENDL, start, end);
    new_line();
}

void Lexer::lex_identifier_or_keyword() {
    assert(is_identifier_start());
    size_t start = pos_;
    advance(1);
    while (is_identifier()) {
        advance(1);
    }
    size_t end = pos_;

    // Assume it is an identifier
    TokenID tok = IDENT;
    string  ident = input_.substr(start, end - start);

    // Look for a keyword, 1-level switch using starting letter
    switch (ident[0]) {
#include "generated/keyword_switch.inc"
    }

    push(tok, start, end);
}

void Lexer::lex_include_filename() {
    assert(at('"') || at('<'));
    size_t start = pos_;
    bool   global = at('<');
    advance(1);

    advance(1);
    while (not at('"') and not at('>') and not at('\n')) {
        advance(1);
    }

    assert((global && at('>')) || at('"'));
    advance(1);

    size_t end = pos_;

    // Push the token
    TokenID tok = global ? GLOBAL_FILENAME : LOCAL_FILENAME;
    push(tok, start, end);
}

void Lexer::lex_int_literal() {
    size_t start = pos_;
    advance(1);  // We already know this is a digit
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
    string ident = input_.substr(start, end - start);
    push(MACRO, start, end);

    // If there is an include, we will parse the following token differently:
    // - If we have '"', it is a LOCAL_FILENAME
    // - If there is a '<', it is a GLOBAL_FILENAME
    // Otherwise, report an error.

    // skip spaces
    while (not finished() && (at(' ') || at('\t'))) {
        advance(1);
    }
    if (finished()) {
        return;
    }
    if (ident == "#include") {
        if (at('"') || at('<')) {
            lex_include_filename();
        } else {
            error("#include must be followed by '\"' or '<'");
        }
    }
}

void Lexer::lex_string_literal() {
    size_t start = pos_;
    advance(1);
    while (not at('"')) {
        advance(1);
    }
    advance(1);  // consume '"'
    size_t end = pos_;
    push(STR_LITERAL, start, end);
}

void Lexer::lex_char_literal() {
    size_t start = pos_;
    advance(1);
    if (at('\\')) {
        advance(1);  // An escape slash
    }
    advance(1);      // The character
    advance(1);      // The closing `'`
    size_t end = pos_;
    push(CHAR_LITERAL, start, end);
}

// Returns `true` if the lexer is not finished yet
void Lexer::run() {
    while (not finished()) {
        if (is_space()) {
            advance(1);
            continue;
        }
        if (is_multiline_comment()) {
            lex_multiline_comment();
            continue;
        }
        if (is_line_comment()) {
            lex_line_comment();
            continue;
        }
        if (is_macro()) {  /// FIXME(pauek): Only at the start of a line!!
            lex_macro();
            continue;
        }
        if (is_identifier_start()) {
            lex_identifier_or_keyword();
            continue;
        }
        if (isdigit(curr())) {
            lex_int_literal();
            continue;
        }
        switch (curr()) {
            case '\r':
            case '\n': {
                lex_endl();
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
                break;
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

                TOKEN_A('!', ADM)
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
                TOKEN_A_AB('^', '=', CARET, CARETEQ)
                TOKEN_A_AB('~', '=', TILDE, TILDEEQ)

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
                char   c = curr();
                string context = input_.substr(max(pos_ - 10, 0U), 20);
                error("Unexpected character '{}' at: \"{}\"", c, context);
            }
        }
    }

    // The size of the last line might not be filled in yet
    if (line_size_ > 0) {
        tokens_.line_size.push_back(line_size_);
    }
}
