#ifndef HL_LEXER_HH
#define HL_LEXER_HH

#include <iostream>
#include <vector>

#include "datatype.hh"
#include "lexer.hh"
#include "tokens.hh"

// High-Level lexing representation (using DataType)

typedef Variant<10000, 10002> TokenGroup;

// A clean sequence of tokens without separators or delimiters
struct TokenSeq : DataType<TokenSeq, 10000> {
    size_t begin, end;
};

struct GroupSeq : DataType<GroupSeq, 10001> {
	size_t begin, end;
	vector<TokenGroup> elems;
};

// <open> ... <close>
struct Structured : DataType<Structured, 10002> {
	TokenID delim = NONE, sep = NONE;
	size_t begin, end;
    vector<Ref<GroupSeq>> groups;
};

// High-Level lexer

class HLexer {
    const Lexer& lexer_;
    size_t pos_;

    Structured root;

    const Tokens& tokens() const { return lexer_.tokens(); }

    bool finished() const { return pos_ >= tokens().ids.size(); }

    TokenID curr() const { return tokens().ids[pos_]; }
    bool    at(TokenID id) const { return curr() == id; }

    bool at_delimiter() const { return is_delimiter(curr()); }
    bool at_separator() const { return is_separator(curr()); }
    bool at_open_delimiter() const { return is_open_delimiter(curr()); }
    bool at_close_delimiter() const { return is_close_delimiter(curr()); }

    bool at_delimiter_or_separator() const {
    	return at_delimiter() || at_separator();
    }

    void advance(size_t n) { pos_ += n; }

    void push(Ref<GroupSeq> group) { root.groups.push_back(group); }

    void skip_space();

    TokenSeq parse_token_seq();
    Ref<Structured> parse_structured(TokenID open, TokenID close);

   public:
    HLexer(const Lexer& lexer) : lexer_(lexer), pos_(0) {}

    void run();
    void dump(std::ostream& out);
};

#endif
