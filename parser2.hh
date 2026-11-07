#ifndef PARSER2_HH
#define PARSER2_HH

#include "astcpp.hh"
#include "hl-lexer.hh"

class Parser2 {
    const HLexer&        hlexer_;
    std::vector<TopDecl> top_decls_;

    TokenID token_id(size_t i) const { return hlexer_.tokens().ids[i]; }
    std::string token_str(size_t i) const { return hlexer_.tokens().str(i); }
    const Tokens& tokens() const { return hlexer_.tokens(); }

    bool is_structured(const TokenGroup& group, TokenID delim, TokenID sep);
    bool is_macro(const GroupSeq& group);
    bool is_function(const GroupSeq& group);

    Ref<TypeSpec> parse_typespec(size_t begin, size_t end);
    Ref<FuncParam> parse_parameter(TokenSeq seq);

    vector<Ref<FuncParam>> parse_parameters(const TokenGroup& group);
    Ref<StmtBlock> parse_block(const TokenGroup& group);
    Ref<Identifier> parse_type_keyword(size_t& i);
    Ref<Identifier> parse_identifier(size_t& i);

    TopDecl parse_function(const GroupSeq& group);
    TopDecl parse_macro(const GroupSeq& group);
    TopDecl parse_top_level(const GroupSeq& group);

   public:
    Parser2(const HLexer& hlexer) : hlexer_(hlexer) {}

    void parse_unit(const Structured& root);


    Ref<Program> run();
};

#endif
