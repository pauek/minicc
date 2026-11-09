#include "parser2.hh"

#include <cassert>

#include "tokens.hh"

bool Parser2::is_structured(
    const TokenGroup&      group,
    TokenID                delim,
    const vector<TokenID>& seps
) {
    if (not group.is<Structured>()) {
        return false;
    }
    auto strd = group.as<Structured>();
    if (strd.delim != delim) {
        return false;
    }
    for (auto sep : seps) {
        if (strd.sep == sep) {
            return true;
        }
    }
    return false;
}

// TokenSeq[USING, NAMESPACE, ...]
bool Parser2::is_using_namespace(const GroupSeq& seq) {
    if (seq.elems.size() < 1 || not seq.elems[0].is<TokenSeq>()) {
        return false;
    }
    auto tokseq = seq.elems[0].as<TokenSeq>();
    return token_id(tokseq.at(0)) == USING && token_id(tokseq.at(1)) == NAMESPACE;
}

// TokenSeq[MACRO, ...]
bool Parser2::is_macro(const GroupSeq& seq) {
    return seq.elems.size() == 1 && seq.elems[0].is<TokenSeq>() &&
           token_id(seq.elems[0].as<TokenSeq>().begin) == MACRO;
}

// TokenSeq[..., IDENT], Structure(,), Structure{;}
bool Parser2::is_function(const GroupSeq& seq) {
    const int num_groups = seq.elems.size();

    // 2 or 3 groups, if it has the body or not (decl/def)
    if (num_groups < 2 || num_groups > 3) {
        return false;
    }

    // The first part is a TokenSeq (typespec + name at the end)
    auto& elm0 = seq.elems[0];
    if (not elm0.is<TokenSeq>() || token_id(elm0.as<TokenSeq>().end - 1) != IDENT) {
        return false;
    }

    // The second group is the parameter list
    if (not is_structured(seq.elems[1], OPEN_PAREN, {COMMA, NONE})) {
        return false;
    }

    // Either we don't have a body...
    if (seq.elems.size() == 2) {
        return true;
    }
    // ... or if we have it, it is a structured like '{;}' (or {} when there are only ifs,
    // fors or whiles)
    return is_structured(seq.elems[2], OPEN_BRACE, {SEMICOLON, NONE});
}

TopDecl Parser2::parse_include(TokenSeq seq) {
    if (seq.length() != 2) {
        error(seq.begin + 2, "Extra characters after filename.");
        return TopDecl();
    }
    size_t file_pos = seq.begin + 1;
    auto   elem1 = token_id(file_pos);
    if (elem1 != LOCAL_FILENAME && elem1 != GLOBAL_FILENAME) {
        error(seq.begin + 1, "Expected a filename.");
        return TopDecl();
    }
    string delim_file = token_str(file_pos);
    return TopDecl(
        Include::make({
            .filename = delim_file.substr(1, delim_file.size() - 2),
            .global = elem1 == GLOBAL_FILENAME,
        })
    );
}

TopDecl Parser2::parse_macro(const GroupSeq& group) {
    assert(group.elems.size() == 1 && group.elems[0].is<TokenSeq>());
    auto tokseq = group.elems[0].as<TokenSeq>();

    string name = token_str(tokseq.begin).substr(1);

    // Special cases
    if (name == "include") {
        return parse_include(tokseq);
    }

    // Any macro
    return TopDecl(
        Macro::make({
            .name = name,
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
    assert(strd.delim == OPEN_PAREN);
    assert(strd.sep == NONE || (strd.sep == COMMA && strd.groups.size() > 1));

    vector<Ref<FuncParam>> params;
    for (auto ref : strd.groups) {
        auto& subgroup = ref.get();
        if (subgroup.elems.size() != 1 || not subgroup.elems[0].is<TokenSeq>()) {
            error(subgroup.begin, "Parameter has more than one token sequence");
        }
        auto tokseq = subgroup.elems[0].as<TokenSeq>();
        params.push_back(parse_parameter(tokseq));
    }

    return params;
}

// Expression Parser

Ref<Identifier> ExprParser::parse_identifier() {
    if (not at(IDENT)) {
        error("Expected identifier.");
    }
    string name = curr_str();
    advance(1);
    return Identifier::make({.name = name});
}

Expr ExprParser::parse_literal() {
    switch (curr()) {
        case INT_LITERAL: {
            int value = stoi(curr_str());
            advance(1);
            return Expr(IntLiteral::make({.value = value}));
        }
        case CHAR_LITERAL: {
            char value = curr_str()[1];
            advance(1);
            return Expr(CharLiteral::make({.value = value}));
        }
        case STR_LITERAL: {
            string value = curr_str();
            advance(1);
            return Expr(StringLiteral::make({.value = value}));
        }
        case TRUE:
        case FALSE: {
            bool value = curr() == TRUE;
            advance(1);
            return Expr(BoolLiteral::make({.value = value}));
        }
        default:
            assert(false);
            return Expr(StringLiteral::make({.value = "ERROR!"}));
    }
}

Expr ExprParser::parse_unary_expr() {
    if (is_literal(curr())) {
        return parse_literal();
    } else if (at(IDENT)) {
        return Expr(parse_identifier());
    } else if (at(OPEN_PAREN)) {
        advance(1);
        auto expr = parse_expr(BinaryExpr::Zero);
        if (not at(CLOSE_PAREN)) {
            error("Expected ')' here.");
        }
        advance(1);
        return expr;
    } else {
        // FIXME(pauek): Add parenthesis here, which reset the level
        error("Unimplemented!");
        return Expr();
    }
}

Expr ExprParser::parse_expr(BinaryExpr::Level break_level) {
    Expr left = parse_unary_expr();
    auto curr_level = BinaryExpr::Infinite;
    while (true) {
        // See the operator, break if not an operator
        BinaryExpr::Operator op = BinaryExpr::token_to_operator_type(curr());
        if (op == BinaryExpr::None) {
            break;
        }

        Expr right;
        auto op_level = BinaryExpr::op_level(op);
        // Break if level is lower
        if (op_level <= break_level) {
            break;
        } else if (op_level > curr_level || curr_level == BinaryExpr::Infinite) {
            advance(1);
            right = parse_expr(op_level);
        } else {
            advance(1);
            curr_level = min(curr_level, op_level);
            right = parse_unary_expr();
        }

        Ref<BinaryExpr> binexpr =
            BinaryExpr::make({.op = op, .left = left, .right = right});
        left = Expr(binexpr);
    }

    return left;
}

// Statements

Stmt Parser2::parse_return(const GroupSeq& group) {
    assert(token_id(group.begin) == RETURN);
    size_t begin = group.begin + 1; // skip 'RETURN' token

    ExprParser::Result result = ExprParser(tokens(), begin, group.end).run();
    if (result.has_error) {
        error(result.error.tokidx, result.error.message);
        return Stmt();
    }
    return Stmt(ReturnStmt::make({.expr = result.expr}));
}

Ref<StmtBlock> Parser2::parse_block(Structured strd) {
    vector<Stmt> stmts;
    for (auto instruction : strd.groups) {
        auto& gr_seq = instruction.get();
        switch (token_id(gr_seq.begin)) {
            case RETURN: {
                stmts.push_back(parse_return(gr_seq));
                break;
            }
            case IF: {
                error(strd.begin, "Unimplemented");
                break;
            }
            case WHILE: {
                error(strd.begin, "Unimplemented");
                break;
            }
            case FOR: {
                error(strd.begin, "Unimplemented");
                break;
            }
            default: {
            }
        }
    }
    return StmtBlock::make({.stmts = stmts});
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
    error(i, "Keyword is not a type!");
    return Identifier::make({});  // NOTE(pauek): Avoid compiler warning
}

Ref<Identifier> Parser2::parse_identifier(size_t& i) {
    if (not token_id(i)) {
        error(i, "Expected identifier.");
    }
    string name = token_str(i);
    i++;
    return Identifier::make({.name = name});
}

Ref<TypeSpec> Parser2::parse_typespec(size_t begin, size_t end) {
    assert(is_keyword(token_id(begin)));

    // Check all possible modifiers
    size_t i = begin;
    int8_t bqual = TypeSpec::None;
    while (i < end && is_typespec_qualifier(token_id(i))) {
        bqual |= TypeSpec::qualifier_from_str(token_str(i));
        i++;
    }

    // Now the main type (for now basic types only)
    Ref<Identifier> ident;
    if (is_keyword(token_id(i))) {
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

    // There are two options, either we have only the header, or it is the function
    // definition
    Ref<StmtBlock> body;
    if (group.elems.size() > 2) {
        // From the `is_function` predicate we know this must be a Structured
        auto strd = group.elems[2].as<Structured>();
        body = parse_block(strd);
    }

    return TopDecl(
        FuncDecl::make({
            .return_type = return_type,
            .ident = ident,
            .params = params,
            .body = body,
        })
    );
}

TopDecl Parser2::parse_using_namespace(const GroupSeq& group) {
    assert(is_using_namespace(group));
    auto tokseq = group.elems[0].as<TokenSeq>();
    if (token_id(tokseq.at(2)) != IDENT) {
        error(tokseq.begin + 2, "Expected a namespace name.");
        return TopDecl();
    }
    string namespc = token_str(tokseq.at(2));
    return TopDecl(Using::make({.namespc = namespc}));
}

TopDecl Parser2::parse_top_level(const GroupSeq& group) {
    if (is_macro(group)) {
        return TopDecl(parse_macro(group));
    } else if (is_function(group)) {
        return TopDecl(parse_function(group));
    } else if (is_using_namespace(group)) {
        return TopDecl(parse_using_namespace(group));
    } else {
        error(group.begin, "Found unknown declaration");
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
