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
        throw new ParseError(tokens_, tokens_.token_line_col(pos_), std::format(fmt, args...));
    }

    void save() { stack_.push(pos_); }

    void forget() { stack_.pop(); }

    void restore() {
        pos_ = stack_.top();
        stack_.pop();
    }

    TokenID     curr() const { return tokens_.ids[pos_]; }
    std::string curr_str() const { return tokens_.str(pos_); }

    char peek(size_t i) const { return tokens_.ids[pos_ + i]; }

    bool at(TokenID tok_id) const { return tokens_.ids[pos_] == tok_id; }
    bool at_str(const char *expected) const { return curr_str() == expected; }

    bool at_typespec_qualifier() const;
    bool at_literal() const;

    bool at(TokenID tok_id1, TokenID tok_id2) const {
        return pos_ + 1 < tokens_.ids.size() && tokens_.ids[pos_] == tok_id1 &&
               tokens_.ids[pos_ + 1] == tok_id2;
    }

    void advance(size_t n) { pos_ += n; }

    void skip_space();

    template <typename T>
    void push(Ref<T> decl) {
        top_decls_.push_back(TopDecl(decl));
    }

    void push(TopDecl top_decl) { top_decls_.push_back(top_decl); }

    BinaryExpr::Operator token_to_operator_type();

    TopDecl         parse_macro();
    Ref<Using>      parse_using();
    Ref<TypeSpec>   parse_typespec();
    Ref<Identifier> parse_type_keyword();
    Ref<Identifier> parse_identifier();

    Expr            parse_expr(BinaryExpr::Level level = BinaryExpr::Zero);
    Expr            parse_unary_expr();
    Expr            parse_literal();
    Ref<BinaryExpr> parse_binary_expr();

    Stmt            parse_stmt();
    Ref<ReturnStmt> parse_return_stmt();
    Ref<AssignStmt> parse_assign_stmt();

    Ref<FuncDecl>          parse_funcdecl();
    vector<Ref<FuncParam>> parse_function_parameters();
    Ref<StmtBlock>         parse_function_body();

    void next();

   public:
    Parser(const Tokens& tokens, std::string filename)
        : tokens_(tokens), filename_(filename), pos_(0) {}

    Ref<Program> run();
};

#endif
