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

// A sequence of token groups ()
struct GroupSeq : DataType<GroupSeq, 10001> {
	size_t begin, end;
	vector<TokenGroup> elems;
};

// A potentially delimited and separated sequence of token groups
//    <OPEN> elem1 <SEP> elem2 <SEP> ... <SEP> elemN <CLOSE>
struct Structured : DataType<Structured, 10002> {
	TokenID delim = NONE, sep = NONE;
	size_t begin, end;
    vector<Ref<GroupSeq>> groups;
};

// High-Level lexer

class HLexer {
    const Lexer& lexer_;
    size_t pos_;

    Structured root_;

    string prefix(int i) const {
        auto [lin, col] = tokens().token_line_col(i);
        return std::format("{}:{}:{}: ", tokens().filename_, lin, col);
    }

    template <class... Args>
    void error(std::format_string<Args...> fmt, Args&&...args) {
        std::cerr << prefix(pos_) << format(fmt, args...) << std::endl;
        exit(1);
    }


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

    void push(Ref<GroupSeq> group) { root_.groups.push_back(group); }

    void skip_space();

    TokenSeq parse_token_seq(TokenID sentinel = NONE, TokenID separator = NONE);
    Ref<Structured> parse_structured(TokenID open, TokenID close, TokenID separator);

   public:
    HLexer(const Lexer& lexer) : lexer_(lexer), pos_(0) {}

    const Tokens& tokens() const { return lexer_.tokens(); }

    void run();
    void dump(std::ostream& out);

    Structured root() const { return root_; }
};

#endif
