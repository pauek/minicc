#ifndef PARSER_HH
#define PARSER_HH

#include <format>
#include <stack>

#include "astcpp.hh"
#include "error.hh"
#include "tokens.hh"

class Parser {
    const Tokens&        tokens_;
    std::vector<TopDecl> top_decls_;
    std::string          filename_;
    size_t               pos_;
    std::stack<size_t>   stack_;

    bool finished() const { return pos_ >= tokens_.ids.size(); }

    template <class... Args>
    void error(std::format_string<Args...> fmt, Args&&...args) {
        throw new ParseError(filename_, tokens_.token_line_col(pos_), std::format(fmt, args...));
    }

    void save() { stack_.push(pos_); }

    void forget() { stack_.pop(); }

    void restore() {
        pos_ = stack_.top();
        stack_.pop();
    }

    TokenID     curr() const { return tokens_.ids[pos_]; }
    std::string curr_str() const { return tokens_.str(pos_); }
    bool        at(TokenID tok_id) const { return tokens_.ids[pos_] == tok_id; }

    void advance(size_t n) { pos_ += n; }

    template <typename T>
    void push(Ref<T> decl) {
        top_decls_.push_back(TopDecl(decl));
    }

    Ref<Macro>    parse_macro();
    Ref<Include>  parse_include();
    Ref<Using>    parse_using();
    Ref<TypeSpec> parse_typespec();

    void next();

   public:
    Parser(Tokens& tokens, std::string filename) : filename_(filename), tokens_(tokens), pos_(0) {}

    Ref<Program> run();
};

#endif
