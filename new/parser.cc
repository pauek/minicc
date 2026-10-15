#include "parser.hh"

#include <string>

#include "astcpp.hh"
#include "tokens.hh"
#include "utils.hh"

void Parser::skip_space() {
    while (at(ENDL)) {
        advance(1);
    }
}

TopDecl Parser::parse_macro() {
    // Macros span a whole line, and we have to take into account the \ (ESC),
    // which disable the ENDL after them.

    // Get the macro name first, and consume it
    string macro_name = tokens_.str(pos_).substr(1);
    advance(1);

    if (macro_name == "include") {
        // Get the filename
        string filename;
        bool   global = false;
        if (at(GLOBAL_FILENAME)) {
            filename = trim(curr_str());
            global = true;
            advance(1);
        } else if (at(LOCAL_FILENAME)) {
            filename = trim(curr_str());
            global = false;
            advance(1);
        } else {
            error("Expected a filename after '#include'.");
        }

        // Expect an ENDL
        if (not at(ENDL)) {
            error("Expected the end of the line here.");
        }
        advance(1);

        return TopDecl(
            Include::make({
                .filename = filename,
                .global = global,
            })
        );
    }

    // Read until the end of the line taking into account ESC tokens.
    size_t start = pos_;
    while (not finished() && not at(ENDL)) {
        // If ESC appears just before ENDL, they cancel each other
        if (at(ESC, ENDL)) {
            advance(2);
        } else {
            advance(1);
        }
    }
    size_t end = pos_;

    return TopDecl(
        Macro::make({
            .name = macro_name,
            .start = start,
            .end = end,
        })
    );
}

Ref<Using> Parser::parse_using() {
    assert(at(KEYWORD) && at_str("using"));
    advance(1);

    if (not at(KEYWORD) || curr_str() != "namespace") {
        error("Expected 'namespace'.");
    }
    advance(1);

    if (not at(IDENT)) {
        error("Expected identifier.");
    }
    string namespc = curr_str();
    advance(1);

    if (not at(SEMICOLON)) {
        error("Expected semicolon.");
    }
    advance(1);

    return Using::make({.namespc = namespc});
}

bool Parser::at_typespec_qualifier() const {
    if (!at(KEYWORD)) {
        return false;
    }
    return at_str("auto") || at_str("const");
}

Ref<Identifier> Parser::parse_type_keyword() {
    assert(at(KEYWORD));
    constexpr const char *basic_types[] = {"bool", "char", "int", "float", "double"};
    for (auto name : basic_types) {
        if (at_str(name)) {
            advance(1);
            return Identifier::make({.name = name});
        }
    }
    error("Keyword is not a type!");
    return Identifier::make({});  // NOTE(pauek): Avoid compiler warning
}

Ref<TypeSpec> Parser::parse_typespec() {
    assert(at(KEYWORD));

    // Check all possible modifiers
    int8_t bqual = TypeSpec::None;
    while (at_typespec_qualifier()) {
        bqual |= TypeSpec::qualifier_from_str(curr_str());
        advance(1);
    }

    // Now the main type (for now basic types only)
    Ref<Identifier> ident;
    if (at(KEYWORD)) {
        ident = parse_type_keyword();
    } else {
        ident = parse_identifier();
    }

    // Now possibly a & reference (pointers not yet)
    bool reference = at(AMP);
    if (reference) {
        advance(1);
    }

    return TypeSpec::make({
        .ident = ident,
        .reference = reference,
        .bqual = bqual,
    });
}

Ref<Identifier> Parser::parse_identifier() {
    assert(at(IDENT));
    string name = curr_str();
    advance(1);
    return Identifier::make({.name = name});
}

vector<Ref<FuncParam>> Parser::parse_function_parameters() {
    assert(at(OPEN_PAREN));
    advance(1);

    vector<Ref<FuncParam>> params;
    while (not at(CLOSE_PAREN)) {
        auto type = parse_typespec();
        auto ident = parse_identifier();
        params.push_back(FuncParam::make({.type = type, .ident = ident}));
        if (at(COMMA)) {
            advance(1);
        }
    }

    advance(1);
    return params;
}

bool Parser::at_literal() const {
    switch (curr()) {
        case INT_LITERAL:
        case STR_LITERAL:
        case CHAR_LITERAL:
            return true;
        default:
            return false;
    }
}

Expr Parser::parse_literal() {
    switch (curr()) {
        case INT_LITERAL: {
            int value = stoi(curr_str());
            advance(1);
            return Expr(IntLiteral::make({.value = value}));
        }
        case CHAR_LITERAL: {
            char value = curr_str()[0];
            advance(1);
            return Expr(CharLiteral::make({.value = value}));
        }
        case STR_LITERAL: {
            string value = curr_str();
            advance(1);
            return Expr(StringLiteral::make({.value = value}));
        }
        default:
            assert(false);
            return Expr(StringLiteral::make({.value = "ERROR!"}));
    }
}

BinaryExpr::Operator Parser::token_to_binary_operator() {
    switch (curr()) {
        case PLUS: {
            return BinaryExpr::Add;
        }
        case MINUS: {
            return BinaryExpr::Sub;
        }
        case STAR: {
            return BinaryExpr::Mul;
        }
        case DIV: {
            return BinaryExpr::Div;
        }
        case MOD: {
            return BinaryExpr::Mod;
        }
        case BARBAR: {
            return BinaryExpr::Or;
        }
        case AMPAMP: {
            return BinaryExpr::And;
        }
        case GT: {
            return BinaryExpr::GT;
        }
        case GEQ: {
            return BinaryExpr::GE;
        }
        case LT: {
            return BinaryExpr::LT;
        }
        case LEQ: {
            return BinaryExpr::LE;
        }
        case EQ: {
            return BinaryExpr::Eq;
        }
        case NEQ: {
            return BinaryExpr::NotEq;
        }
        default: {
            return BinaryExpr::None;
        }
    }
}

Ref<BinaryExpr> Parser::parse_binary_expr() {
    BinaryExpr binexp;

    // Left Hand Side
    if (at(IDENT)) {
        binexp.left = Expr(parse_identifier());
    } else if (at_literal()) {
        binexp.left = parse_literal();
    } else {
        error("Unimplemented!");
    }

    // Operator
    binexp.op = token_to_binary_operator();
    advance(1);

    // Right Hand Side
    if (at(IDENT)) {
        binexp.right = Expr(parse_identifier());
    } else {
        error("Unimplemented!");
    }

    return BinaryExpr::make(binexp);
}

Expr Parser::parse_expr(BinaryExpr::Level level) {
    Expr left;

    // Parse the left hand side
    if (at_literal()) {
        left = parse_literal();
    } else if (at(IDENT)) {
        left = Expr(parse_identifier());
    } else {
        // FIXME(pauek): Add parenthesis here, which reset the level
        error("Unimplemented!");
    }

    // See the operator
    BinaryExpr::Operator op = token_to_binary_operator();

    // The expression ends here if the token is not an operator...
    if (op == BinaryExpr::None) {
        return left;
    }
    // ... or the level of the operator is less than the level of the expression
    if (BinaryExpr::op_level(op) < level) {
        return left;
    }
    advance(1);

    // We have a binary expression, only the right hand side is missing
    Expr right = parse_expr(level);

    Ref<BinaryExpr> ref = BinaryExpr::make({
        .level = level,
        .op = op,
        .left = left,
        .right = right,
    });

    return Expr(ref);
}

Ref<ReturnStmt> Parser::parse_return_stmt() {
    assert(at(KEYWORD) && at_str("return"));
    advance(1);

    Expr expr = parse_expr();

    // Expect semicolon
    if (not at(SEMICOLON)) {
        error("Expected semicolon.");
    }
    advance(1);

    // Consume ENDL if present
    if (at(ENDL)) {
        advance(1);
    }

    return ReturnStmt::make({.expr = expr});
}

Ref<AssignStmt> Parser::parse_assign_stmt() {
    assert(at(IDENT));
    auto target = parse_identifier();

    assert(at(EQ));
    advance(1);

    Expr expr = parse_expr();
    if (not at(SEMICOLON)) {
        error("Expected semicolon");
    }
    advance(1);

    return AssignStmt::make({.target = target, .expr = expr});
}

Stmt Parser::parse_stmt() {
    Stmt empty;
    switch (curr()) {
        case KEYWORD: {
            if (curr_str() == "return") {
                return Stmt(parse_return_stmt());
            } else {
                return empty;
            }
            break;
        }
        case IDENT: {
            // If we find an identifier, we have to look further
            if (peek(1) == EQ) {
                return Stmt(parse_assign_stmt());
            }
        }
        default: {
            string tok = curr_str();
            error("Unexpected token '{}'", tok);
            return empty;
        }
    }
}

Ref<StmtBlock> Parser::parse_function_body() {
    assert(at(OPEN_BRACE));
    advance(1);  // Consume '{'

    skip_space();

    StmtBlock block;
    while (not at(CLOSE_BRACE)) {
        auto stmt = parse_stmt();
        block.stmts.push_back(stmt);
        skip_space();
    }
    advance(1);  // Consume '}'

    return StmtBlock::make(block);
}

Ref<FuncDecl> Parser::parse_funcdecl() {
    auto           return_type = parse_typespec();
    auto           ident = parse_identifier();
    auto           params = parse_function_parameters();
    Ref<StmtBlock> body;

    // Now if we find a '{', the function is defined here
    if (at(OPEN_BRACE)) {
        body = parse_function_body();
    } else if (at(SEMICOLON)) {
        advance(1);
    } else {
        error("Expected the function body or a semicolon here.");
    }
    return FuncDecl::make({
        .return_type = return_type,
        .ident = ident,
        .params = params,
        .body = body,
    });
}

void Parser::next() {
    while (true) {
        if (finished()) {
            break;
        }
        switch (curr()) {
            case ENDL: {
                advance(1);
                break;
            }
            case MACRO: {
                push(parse_macro());
                break;
            }
            case KEYWORD: {
                string id = curr_str();

                // Using declaration
                if (id == "using") {
                    push(parse_using());
                    break;
                }

                // Function Declaration (for now)
                push(parse_funcdecl());
                break;
            }
            default: {
                string tok = curr_str();
                error("Unexpected token '{}'.", tok);
            }
        }
    }
}

Ref<Program> Parser::run() {
    next();
    while (not finished()) {
        next();
    }
    return Program::make({.declarations = top_decls_});
}
