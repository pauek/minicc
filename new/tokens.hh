#ifndef TOKENS_HH
#define TOKENS_HH

#include <cstdint>
#include <iostream>
#include <vector>

// Line and Column

struct LinCol {
    uint32_t lin, col;
};

inline std::ostream& operator<<(std::ostream& out, LinCol& lincol) {
    return out << lincol.lin << ':' << lincol.col;
}

// TokenID

enum TokenID {
#define TOKEN(id, name) id,
#include "tokens.inc"
#undef TOKEN
};

std::string token_name(TokenID id);

//
// Token sequence
//
struct Tokens {
    const std::string& input_;
    std::string        filename_;

    std::vector<TokenID>  ids;
    std::vector<uint32_t> start_pos, end_pos;
    std::vector<uint32_t> line_size;

    Tokens(const std::string& input, std::string filename = "<unknown>")
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

    std::string name(size_t i) const { return token_name(ids[i]); }

    void dump(std::ostream& out) const;
};

#endif
