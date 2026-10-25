#ifndef LEXER_HH
#define LEXER_HH

#include <cctype>
#include <cstdint>
#include <format>
#include <string>

#include "error.hh"
#include "tokens.hh"

class Lexer {
    const std::string& input_;
    std::string        filename_;
    Tokens             tokens_;

    uint32_t pos_, line_size_;

    template <class... Args>
    void error(std::format_string<Args...> fmt, Args&&...args) {
        throw new LexError(filename_, tokens_.line_col(pos_), std::format(fmt, args...));
    }

    const std::string& input() const { return input_; }

    char peek(size_t i) const { return input_[pos_ + i]; }
    char curr() const { return input_[pos_]; }

    bool at(char c) const { return curr() == c; }
    bool at(char c1, char c2) const { return curr() == c1 && peek(1) == c2; }

    bool finished() const { return pos_ >= input_.size(); }
    bool is_line_comment() const { return at('/', '/'); }
    bool is_multiline_comment() const { return at('/', '*'); }
    bool is_macro() const { return curr() == '#'; }
    bool is_space() const { return curr() == ' ' || curr() == '\t'; }

    bool is_identifier_start() const { return isupper(curr()) || islower(curr()) || curr() == '_'; }

    bool is_identifier() const {
        return isupper(curr()) || islower(curr()) || isdigit(curr()) || curr() == '_';
    }

    /**
     * Advance the position of the lexer by `n`
     */
    void advance(size_t n) {
        pos_ += n;
        line_size_ += n;
    }

    void new_line() {
        tokens_.line_size.push_back(line_size_);
        line_size_ = 0;
    }

    void push(TokenID tokid, size_t start, size_t end);

    void lex_endl();
    void lex_line_comment();
    void lex_multiline_comment();

    void lex_macro();
    void lex_include_filename();

    void lex_int_literal();
    void lex_identifier_or_keyword();
    void lex_char_literal();
    void lex_string_literal();

   public:
    Lexer(const std::string& input, std::string filename)
        : input_(input), filename_(filename), tokens_(input, filename) {
        pos_ = 0;
        line_size_ = 0;
    }

    void run();

    Tokens& tokens() { return tokens_; }
    const Tokens& tokens() const { return tokens_; }
};

#endif
