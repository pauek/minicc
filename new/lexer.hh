#ifndef LEXER_HH
#define LEXER_HH

#include <cctype>
#include <cstdint>
#include <format>
#include <iostream>
#include <string>

#include "tokens.hh"

inline std::ostream& operator<<(std::ostream& out, LinCol& lincol) {
    return out << lincol.lin << ':' << lincol.col;
}

struct LexerError {
    LinCol      pos;
    std::string msg;

   public:
    LexerError(LinCol pos_, std::string msg_) : pos(pos_), msg(msg_) {}
};

class Lexer {
    const std::string& input_;
    Tokens             tokens_;

    uint32_t pos_, line_size_;
    uint32_t curr_token_;

    template <class... Args>
    void error(std::format_string<Args...> fmt, Args&&...args) {
        throw new LexerError(tokens_.line_col(pos_), std::format(fmt, args...));
    }

    char peek(size_t i) const { return input_[pos_ + i]; }
    char curr() const { return input_[pos_]; }

    bool finished() const { return pos_ >= input_.size(); }
    bool is_line_comment() const { return curr() == '/' && peek(1) == '/'; }
    bool is_multiline_comment() const { return curr() == '/' && peek(1) == '*'; }
    bool is_macro() const { return curr() == '#'; }
    bool is_space() const { return curr() == ' ' || curr() == '\t'; }

    bool is_identifier_start() const {
        return isupper(curr()) || islower(curr()) || curr() == '_';
    }

    bool is_identifier() const {
        return isupper(curr()) || islower(curr()) || isdigit(curr()) || curr() == '_';
    }

    void advance(size_t n) {
        pos_ += n;
        line_size_ += n;
    }

    void push(TokenID tokid, size_t start, size_t end);

    void lex_line_comment();

    void lex_macro();

    void lex_int_literal();
    void lex_identifier();
    void lex_char_literal();
    void lex_string_literal();

    void next();

   public:
    Lexer(const std::string& input, std::string filename)
        : input_(input), tokens_(input, filename) {
        pos_ = 0;
        curr_token_ = 0;
        line_size_ = 0;
    }

    Tokens run();
};

#endif
