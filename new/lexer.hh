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

    void push(TokenID tokid, size_t start, size_t end) {
        tokens_.ids.push_back(tokid);
        tokens_.start_pos.push_back(start);
        tokens_.end_pos.push_back(end);
        size_t jump = end - pos_;
        pos_ = end;
        line_size_ += jump;
    }

    char at(size_t offset) { return input_[pos_ + offset]; }

    bool finished() const { return pos_ >= input_.size(); }

    bool is_comment() const { return curr() == '/' && peek(1) == '/'; }

    bool is_identifier_start() const {
        return isupper(curr()) || islower(curr()) || isdigit(curr()) || curr() == '.';
    }

    bool is_space() const { return curr() == ' ' || curr() == '\t'; }

    bool is_identifier() const {
        return isupper(curr()) || islower(curr()) || isdigit(curr()) || curr() == '_' ||
               curr() == '.' || curr() == '-';
    }

    void advance(size_t n) {
        pos_ += n;
        line_size_ += n;
    }

    char curr() const { return input_[pos_]; }
    char peek(size_t i) const { return input_[pos_ + i]; }

    void skip_line_comment() {
        advance(2); // Skip "//"
        while (curr() != '\n') {
            advance(1);
        }
        // FIXME(pauek): Push the comment as a token!
        push(ENDL, pos_, pos_ + 1);
        tokens_.line_size.push_back(line_size_);
        line_size_ = 0;
    }

    void lex_identifier() {
        size_t start = pos_;
        advance(1);
        while (is_identifier()) {
            advance(1);
        }

        // We allow for identifiers to have a '![0-9]+' tail,
        // which for now does nothing but will allow to embed IDs
        // in the file. Parsing, normalizing and then printing gives
        // a new file with normalized ids which can be read into
        // a clean Database without normalization needed.
        if (curr() == '!') {
            // Ignore the index for now
            advance(1);
            while (isdigit(curr())) {
                advance(1);
            }
        }

        size_t end = pos_;
        push(IDENT, start, end);
    }

    void lex_string_literal() {
        size_t start = pos_;
        advance(1);
        while (curr() != '"') {
            advance(1);
        }
        advance(1);  // consume '"'
        size_t end = pos_;
        push(LITERAL_STR, start, end);
    }

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
