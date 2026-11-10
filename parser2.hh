#ifndef PARSER2_HH
#define PARSER2_HH

#include "astcpp.hh"
#include "config.hh"
#include "hl-lexer.hh"

class Parser2 {
    struct Error {
        LinCol pos;
        string message;
    };

    const HLexer&        hlexer_;
    std::vector<TopDecl> top_decls_;

    std::vector<Error> errors_;

    void error(size_t tokpos, string message) {
        LinCol filepos = hlexer_.tokens().token_line_col(tokpos);
        errors_.push_back({.pos = filepos, .message = message});
    }

    TokenID       token_id(size_t i) const { return hlexer_.tokens().ids[i]; }
    std::string   token_str(size_t i) const { return hlexer_.tokens().str(i); }
    const Tokens& tokens() const { return hlexer_.tokens(); }

    bool is_structured(
        const TokenGroup&      group,
        TokenID                delim,
        const vector<TokenID>& sep = {NONE}
    );
    bool is_macro(const GroupSeq& group);
    bool is_function(const GroupSeq& group);
    bool is_global_vardecl(const GroupSeq& group);
    bool is_using_namespace(const GroupSeq& seq);

    Ref<TypeSpec>  parse_typespec(size_t begin, size_t end);
    Ref<FuncParam> parse_parameter(TokenSeq seq);

    vector<Ref<FuncParam>> parse_parameters(const TokenGroup& group);
    Ref<StmtBlock>         parse_block(Structured strd);
    Ref<Identifier>        parse_type_keyword(size_t& i);
    Ref<Identifier>        parse_identifier(size_t& i);
    Ref<VarDecl>           parse_vardecl(const TokenSeq& seq);

    // Statements
    Stmt parse_return(const GroupSeq& group);

    TopDecl parse_include(TokenSeq seq);
    TopDecl parse_macro(const GroupSeq& group);
    TopDecl parse_using_namespace(const GroupSeq& group);
    TopDecl parse_function(const GroupSeq& group);
    TopDecl parse_global_vardecl(const GroupSeq& group);
    TopDecl parse_top_level(const GroupSeq& group);

   public:
    Parser2(const HLexer& hlexer) : hlexer_(hlexer) {}

    void parse_unit(const Structured& root);
    bool has_errors() const { return not errors_.empty(); }

    const vector<Error>& errors() const { return errors_; }

    Ref<Program> run();
};

class ExprParser {
    const Tokens& tokens_;
    size_t        begin_, end_, pos_;

    TokenID curr() const { return tokens_.ids[pos_]; }
    string  curr_str() const { return tokens_.str(pos_); }

    bool at(TokenID id) const { return curr() == id; }

    void advance(size_t n) { pos_ += n; }

    Ref<Identifier> parse_identifier();
    Expr            parse_literal();
    Expr            parse_base_expr();
    Expr            parse_unary_expr();

    Expr parse_expr(BinaryExpr::Level break_level);

   public:
    ExprParser(const Tokens& tokens, size_t begin, size_t end)
        : tokens_(tokens), begin_(begin), end_(end) {
        // We are going to actually ignore the groups inside `group`, even
        // the tree is partially there (if there are parentheses), but it is
        // easier to process the whole thing linearly.
        pos_ = begin_;
    }

    struct Error {
        string message;
        size_t tokidx;
    };

    struct Result {
        Expr  expr;
        bool  has_error = false;
        Error error;
    };

    // Whether the expression parser has consumed all tokens
    bool is_complete() const { return pos_ == end_; }

    size_t pos() const { return pos_; }

    Result run() {
        try {
            Result result;
            result.expr = parse_expr(BinaryExpr::Level::Zero);
            return result;
        } catch (Error *e) {
            return {.expr = Expr(), .has_error = true, .error = *e};
        }
    }
};

#endif
