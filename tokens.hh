#ifndef TOKENS_HH
#define TOKENS_HH

#include <cassert>
#include <cstdint>
#include <iostream>
#include <vector>

// Line and Column

struct LinCol {
    uint32_t lin, col;
};

inline std::ostream& operator<<(std::ostream& out, const LinCol& lincol) {
    return out << lincol.lin << ':' << lincol.col;
}

// Low-Level TokenID
enum TokenID {
    NONE = 0,

#define TOKEN(id, name) id,
#include "tokens.inc"
#undef TOKEN

    BEFORE_KEYWORDS,
#define KEYWORD(id, name) id,
#include "generated/keywords.inc"
#undef KEYWORD
    AFTER_KEYWORDS,

};

inline bool is_separator(TokenID id) {
    return id == SEMICOLON || id == COMMA;
}

inline bool is_open_delimiter(TokenID id) {
    return id == OPEN_BRACE || id == OPEN_PAREN || id == OPEN_BRACKET;
}
inline bool is_close_delimiter(TokenID id) {
    return id == CLOSE_BRACE || id == CLOSE_PAREN || id == CLOSE_BRACKET;
}
inline bool is_delimiter(TokenID id) {
    return is_open_delimiter(id) || is_close_delimiter(id);
}

inline TokenID closing(TokenID open) {
    switch (open) {
        case OPEN_PAREN:
            return CLOSE_PAREN;
        case OPEN_BRACE:
            return CLOSE_BRACE;
        case OPEN_BRACKET:
            return CLOSE_BRACKET;
        default:
            assert(false);
    }
}

// High-Level TokenID
enum HLTokenID {
#define HLTOKEN(id) id,
#include "hl-tokens.inc"
#undef HLTOKEN
};

std::string token_name(TokenID id);

inline bool is_keyword(TokenID id) {
    return id > BEFORE_KEYWORDS && id < AFTER_KEYWORDS;
}

//
// Token sequence
//
struct Tokens {
    const std::string& input_;
    std::string        filename_;

    std::vector<TokenID>  ids;
    std::vector<uint32_t> start_pos, end_pos;
    std::vector<uint32_t> line_size;

    explicit Tokens(const std::string& input, std::string filename = "<unknown>")
        : input_(input), filename_(filename) {}

    // Determine the line and column of a certain target position in the buffer
    LinCol line_col(uint32_t target) const {
        uint32_t iline = 0, pos = 0;
        while (iline < line_size.size() && pos + line_size[iline] <= target) {
            pos += line_size[iline];
            iline++;
        }
        return {1 + iline, 1 + target - pos};
    }

    LinCol token_line_col(size_t i) const { return line_col(start_pos[i]); }

    std::string str(size_t i) const {
        const auto start = start_pos[i];
        const auto end = end_pos[i];
        return input_.substr(start, end - start);
    }

    std::string get_line(size_t k) const {
        // Add line lengths before line k
        size_t begin = 0;
        for (size_t i = 0; i < k - 1; i++) {
            assert(i < line_size.size());
            begin += line_size[i];
        }
        return input_.substr(begin, line_size[k - 1]);
    }

    std::string name(size_t i) const { return token_name(ids[i]); }

    void push(TokenID id, size_t start, size_t end) {
        ids.push_back(id);
        start_pos.push_back(start);
        end_pos.push_back(end);
    }

    void dump(std::ostream& out) const;
};

#endif
