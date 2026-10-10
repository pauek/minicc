#ifndef TOKENS_HH
#define TOKENS_HH

#include <cstdint>
#include <iostream>
#include <vector>

enum TokenID {

#define TOKEN(id, name) id,
#include "tokens.inc"
#undef TOKEN
};

struct LinCol {
	uint32_t lin, col;
};

std::string token_name(TokenID id);

//
// Token sequence
//
struct Tokens {
	const std::string& input_;
	std::string filename_;

	std::vector<TokenID> ids;
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

	std::string ident_str(size_t i) const {
		// This is like "str" but it stops at the '!' sign, to allow
		// names to include the ID, which is not really part of the name
		const auto start = start_pos[i];
		const auto end = end_pos[i];
		uint32_t pos = start;
		while (pos < end && input_[pos] != '!') {
			pos++;
		}
		return input_.substr(start, pos - start);
	}

	std::string name(size_t i) const { return token_name(ids[i]); }

	void dump(std::ostream& out) const {
		for (size_t i = 0; i < ids.size(); i++) {
			auto pos = token_line_col(i);
			out << filename_ << ':' << pos.lin << ':' << pos.col << ": ";

			const auto code = ids[i];
			if (code == IDENT) {
				out << "identifier \"" << ident_str(i) << '"';
			} else if (code == LITERAL_STR) {
				out << "literal " << ident_str(i);
			} else {
				out << name(i);
			}
			out << std::endl;
		}
	}
};

#endif
