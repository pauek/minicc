#include "parser2.hh"

#include <cassert>

#include "tokens.hh"

bool Parser2::is_structured(const TokenGroup& group, TokenID delim, TokenID sep) {
    if (not group.is<Structured>()) {
        return false;
    }
    auto strd = group.as<Structured>();
    return (strd.delim != delim || strd.sep != sep);
}

// TokenSeq[MACRO, ...]
bool Parser2::is_macro(const GroupSeq& seq) {
    return seq.elems.size() == 1 && seq.elems[0].is<TokenSeq>() &&
           token_id(seq.elems[0].as<TokenSeq>().begin) == MACRO;
}

// TokenSeq[..., IDENT], Structure(,), Structure{;}
bool Parser2::is_function(const GroupSeq& seq) {
    return seq.elems.size() == 3 && seq.elems[0].is<TokenSeq>() &&
           token_id(seq.elems[0].as<TokenSeq>().end - 1) == IDENT &&
           is_structured(seq.elems[1], OPEN_PAREN, COMMA) &&
           is_structured(seq.elems[2], OPEN_BRACE, SEMICOLON);
}

TopDecl Parser2::parse_macro(const GroupSeq& group) {
    assert(group.elems.size() == 1 && group.elems[0].is<TokenSeq>());
    auto tokseq = group.elems[0].as<TokenSeq>();
    return TopDecl(
        Macro::make({
            .name = token_str(tokseq.begin).substr(1),
            .start = tokseq.begin + 1,
            .end = tokseq.end,
        })
    );
}

Ref<FuncParam> Parser2::parse_parameter(TokenSeq seq) {
    auto name = token_str(seq.last());
    auto typespec = parse_typespec(seq.begin, seq.last());
    return FuncParam::make({
        .type = typespec,
        .ident = Identifier::make({.name = name}),
    });
}

vector<Ref<FuncParam>> Parser2::parse_parameters(const TokenGroup& group) {
    assert(group.is<Structured>());
    auto strd = group.as<Structured>();
    assert(strd.delim == OPEN_PAREN && strd.sep == COMMA);

    vector<Ref<FuncParam>> params;
    for (auto ref : strd.groups) {
        auto& subgroup = ref.get();
        if (subgroup.elems.size() != 1 || not subgroup.elems[0].is<TokenSeq>()) {
            error("Parameter has more than one token sequence");
        }
        auto tokseq = subgroup.elems[0].as<TokenSeq>();
        params.push_back(parse_parameter(tokseq));
    }

    return params;
}

Ref<StmtBlock> Parser2::parse_block(const TokenGroup& group) {
    return StmtBlock::make({.stmts = {}});
}

Ref<Identifier> Parser2::parse_type_keyword(size_t& i) {
    assert(is_type_keyword(token_id(i)));
    constexpr const char *basic_types[] = {"bool", "char", "int", "float", "double"};
    for (auto name : basic_types) {
        if (token_str(i) == name) {
            i++;
            return Identifier::make({.name = name});
        }
    }
    error("Keyword is not a type!");
    return Identifier::make({});  // NOTE(pauek): Avoid compiler warning
}

Ref<Identifier> Parser2::parse_identifier(size_t& i) {
    if (not token_id(i)) {
        error("Expected identifier.");
    }
    string name = token_str(i);
    i++;
    return Identifier::make({.name = name});
}

Ref<TypeSpec> Parser2::parse_typespec(size_t begin, size_t end) {
    assert(token_id(begin) == KEYWORD);

    // Check all possible modifiers
    size_t i = begin;
    int8_t bqual = TypeSpec::None;
    while (i < end && is_typespec_qualifier(token_id(i))) {
        bqual |= TypeSpec::qualifier_from_str(token_str(i));
        i++;
    }

    // Now the main type (for now basic types only)
    Ref<Identifier> ident;
    if (token_id(i) == KEYWORD) {
        ident = parse_type_keyword(i);
    } else {
        ident = parse_identifier(i);
    }

    // Now possibly a & reference (pointers not yet)
    bool reference = token_id(i) == AMP;
    if (reference) {
        i++;
    }

    return TypeSpec::make({
        .ident = ident,
        .reference = reference,
        .bqual = bqual,
    });
}

TopDecl Parser2::parse_function(const GroupSeq& group) {
    assert(is_function(group));
    auto tokseq = group.elems[0].as<TokenSeq>();

    auto return_type = parse_typespec(tokseq.begin, tokseq.last());
    auto ident = Identifier::make({.name = token_str(tokseq.last())});
    auto params = parse_parameters(group.elems[1]);
    auto body = parse_block(group.elems[2]);

    return TopDecl(
        FuncDecl::make({
            .return_type = return_type,
            .ident = ident,
            .params = params,
            .body = body,
        })
    );
}

TopDecl Parser2::parse_top_level(const GroupSeq& group) {
    if (is_macro(group)) {
        return TopDecl(parse_macro(group));
    } else if (is_function(group)) {
        return TopDecl(parse_function(group));
    } else {
        return TopDecl();
    }
}

void Parser2::parse_unit(const Structured& root) {
    for (auto group : root.groups) {
        top_decls_.push_back(parse_top_level(group.get()));
    }
}

Ref<Program> Parser2::run() {
    const auto& root = hlexer_.root();
    parse_unit(root);
    return Program::make({.declarations = top_decls_});
}
