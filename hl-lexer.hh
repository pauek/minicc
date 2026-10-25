#ifndef HL_LEXER_HH
#define HL_LEXER_HH

#include <iostream>

#include "lexer.hh"
#include "tokens.hh"

class HLLexer {
    Lexer& lexer_;
    size_t pos_;

    const Tokens& tokens() const { return lexer_.tokens(); }
    Tokens&       tokens() { return lexer_.tokens(); }

    bool finished() const { return pos_ < tokens().ids.size(); }

    TokenID curr() const { return tokens().ids[pos_]; }
    bool    at(TokenID id) const { return curr() == id; }

    bool at_punct() {
        return at(CLOSE_PAREN) || at(CLOSE_BRACE) || at(CLOSE_BRACKET) || at(SEMICOLON) ||
               at(OPEN_PAREN) || at(OPEN_BRACE) || at(OPEN_BRACKET) || at(COMMA);
    }

    void advance(size_t n) { pos_ += n; }

   public:
    HLLexer(Lexer& lexer) : lexer_(lexer), pos_(0) {}

    void run();

    void parse_token_group();
    void parse_brackets();
    void parse_parentheses();
    void parse_block();

    void dump(std::ostream& out);
};

#endif
