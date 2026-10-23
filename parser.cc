#include "parser.hh"

#include <string>

#include "config.hh"
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
    assert(at(USING));
    advance(1);

    if (not at(NAMESPACE)) {
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

bool Parser::at_type_keyword() const {
    return at(BOOL) || at(CHAR) || at(INT) || at(FLOAT) || at(DOUBLE);
}

bool Parser::at_typespec_qualifier() const {
    if (at(AUTO) || at(CONST)) {
        return true;
    }
    return false;
}

Ref<Identifier> Parser::parse_type_keyword() {
    assert(at_type_keyword());
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
    assert(at_keyword());

    // Check all possible modifiers
    int8_t bqual = TypeSpec::None;
    while (at_typespec_qualifier()) {
        bqual |= TypeSpec::qualifier_from_str(curr_str());
        advance(1);
    }

    // Now the main type (for now basic types only)
    Ref<Identifier> ident;
    if (at_keyword()) {
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
    if (not at(IDENT)) {
        error("Expected identifier.");
    }
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
        case TRUE:
        case FALSE:
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

BinaryExpr::Operator Parser::token_to_operator_type() {
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
        case EQEQ: {
            return BinaryExpr::Eq;
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
    binexp.op = token_to_operator_type();
    advance(1);

    // Right Hand Side
    if (at(IDENT)) {
        binexp.right = Expr(parse_identifier());
    } else {
        error("Unimplemented!");
    }

    return BinaryExpr::make(binexp);
}

Expr Parser::parse_unary_expr() {
    if (at_literal()) {
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

Expr Parser::parse_expr(BinaryExpr::Level break_level) {
    Expr left = parse_unary_expr();
    auto curr_level = BinaryExpr::Infinite;
    while (true) {
        // See the operator, break if not an operator
        BinaryExpr::Operator op = token_to_operator_type();
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

        Ref<BinaryExpr> binexpr = BinaryExpr::make({.op = op, .left = left, .right = right});
        left = Expr(binexpr);
    }

    return left;
}

Ref<ReturnStmt> Parser::parse_return_stmt() {
    assert(at(RETURN));
    advance(1);
    ReturnStmt stmt;
    stmt.expr = parse_expr();
    return ReturnStmt::make(stmt);
}

Ref<IncrExpr> Parser::parse_incr_expr() {
    IncrExpr incr;
    if (IncrExpr::is_incr_op(curr())) {
        incr.order = IncrExpr::Pre;
        incr.op = IncrExpr::token_to_op(curr());
        advance(1);
        if (not at(IDENT)) {
            error("Expected identifier.");
        }
        incr.target = parse_identifier();
    } else if (at(IDENT)) {
        incr.order = IncrExpr::Post;
        incr.target = parse_identifier();
        // If we did call parse_incr_stmt we should have known that
        // there was an incr operator after the identifier...
        assert(IncrExpr::is_incr_op(curr()));
        incr.op = IncrExpr::token_to_op(curr());
        advance(1);
    } else {
        assert(false);
    }
    return IncrExpr::make(incr);
}

Ref<IncrStmt> Parser::parse_incr_stmt() {
    auto expr = parse_incr_expr();
    return IncrStmt::make({.incr = expr});
}

Ref<AssignStmt> Parser::parse_assign_stmt() {
    if (Config::get<Config::instructions::assignment>() == Config::No) {
        error("Assignment is not allowed with current config.");
    }
    assert(at(IDENT));
    AssignStmt stmt;
    stmt.assign = parse_assign_expr();
    return AssignStmt::make(stmt);
}

Ref<CondExpr> Parser::parse_cond_expr() {
    CondExpr expr;

    expect(OPEN_PAREN, "Expected '('.");
    expr.cond = parse_expr();
    expect(CLOSE_PAREN, "Expected ')'.");
    expr.then = parse_stmt_block();

    return CondExpr::make(expr);
}

Ref<IfStmt> Parser::parse_if_stmt() {
    assert(at(IF));
    advance(1);

    IfStmt stmt;
    stmt.cond_exprs.push_back(parse_cond_expr());
    while (at(ELSE, IF)) {
        advance(2);
        stmt.cond_exprs.push_back(parse_cond_expr());
    }
    if (at(ELSE)) {
        advance(1);
        stmt.els = parse_stmt_block();
    }

    return IfStmt::make(stmt);
}

Ref<WhileStmt> Parser::parse_while_stmt() {
    assert(at(WHILE));
    advance(1);

    WhileStmt stmt;

    // Condition
    expect(OPEN_PAREN, "Expected '('.");
    stmt.cond = parse_expr();
    expect(CLOSE_PAREN, "Expected ')'.");

    // Body
    stmt.body = parse_stmt_block();

    return WhileStmt::make(stmt);
}

Ref<AssignExpr> Parser::parse_assign_expr() {
    AssignExpr assign;
    assign.target = parse_identifier();
    // Initialization is optional
    if (at_assign_token()) {
        assign.op = AssignExpr::token_to_op(curr());
        advance(1);
        assign.expr = parse_expr();
    }
    return AssignExpr::make(assign);
}

Ref<ForInitDecl> Parser::parse_for_init_decl() {
    auto                       type = parse_typespec();
    vector<Ref<ForInitAssign>> variables;
    while (not at(SEMICOLON)) {
        auto var = parse_for_init_assignment();
        variables.push_back(var);
        if (at(COMMA)) {
            advance(1);
        }
    }
    return ForInitDecl::make({.type = type, .variables = variables});
}

Ref<ForInitAssign> Parser::parse_for_init_assignment() {
    assert(at(IDENT));

    AssignExpr assign;
    assign.target = parse_identifier();
    expect(EQ, "Expected '='.");
    assign.op = AssignExpr::Normal;
    assign.expr = parse_expr();

    auto ref = AssignExpr::make(assign);
    return ForInitAssign::make({.assign = ref});
}

ForIncrExpr Parser::parse_for_incr_expr() {
    if (IncrExpr::is_incr_op(curr()) || (at(IDENT) && IncrExpr::is_incr_op(peek(1)))) {
        return ForIncrExpr(parse_incr_expr());
    } else if (at(IDENT) && AssignExpr::is_assign_op(peek(1))) {
        return ForIncrExpr(parse_assign_expr());
    } else {
        error("Expected assignment or increment.");
        return ForIncrExpr();
    }
}

ForInitStmt Parser::parse_for_init() {
    if (at_typespec_qualifier() || at_type_keyword()) {
        return ForInitStmt(parse_for_init_decl());
    } else if (at(IDENT)) {
        return ForInitStmt(parse_for_init_assignment());
    } else {
        error("Expected declaration or assignment.");
        return ForInitStmt();  // Muffle compiler
    }
}

Ref<ForStmt> Parser::parse_for_stmt() {
    assert(at(FOR));
    advance(1);

    ForStmt stmt;

    // Triple expression
    expect(OPEN_PAREN, "Expected '('.");
    stmt.init = parse_for_init();
    expect(SEMICOLON, "Expected ';'.");
    stmt.cond = parse_expr();
    expect(SEMICOLON, "Expected ';'.");
    stmt.incr = parse_for_incr_expr();
    expect(CLOSE_PAREN, "Expected ')'.");

    // Body
    stmt.body = parse_stmt_block();

    return ForStmt::make(stmt);
}

Stmt Parser::parse_stmt() {
    Stmt empty;
    switch (curr()) {
        case RETURN: {
            return Stmt(parse_return_stmt());
        }
        case IF: {
            return Stmt(parse_if_stmt());
        }
        case WHILE: {
            return Stmt(parse_while_stmt());
        }
        case FOR: {
            return Stmt(parse_for_stmt());
        }
        case PLUSPLUS:
        case MINUSMINUS: {
            return Stmt(parse_incr_stmt());
        }
        case IDENT: {
            TokenID next = peek(1);
            if (AssignExpr::is_assign_op(next)) {
                return Stmt(parse_assign_stmt());
            } else if (IncrExpr::is_incr_op(next)) {
                return Stmt(parse_incr_stmt());
            }
            error("Unexpected token.");
            return empty;
        }
        default: {
            error("Unexpected token.");
            return empty;
        }
    }
}

Ref<StmtBlock> Parser::parse_stmt_block() {
    assert(at(OPEN_BRACE));
    advance(1);  // Consume '{'

    skip_space();

    StmtBlock block;
    while (not at(CLOSE_BRACE)) {
        auto stmt = parse_stmt();
        if (peek(-1) != CLOSE_BRACE) {  // NOTE(pauek): Inelegant??? Probably...
            expect(SEMICOLON, "Expected ';'.");
        }
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
        body = parse_stmt_block();
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
            case USING: {
                push(parse_using());
                break;
            }
            case TYPEDEF: {
                error("Unimplemented!");
            }
            case STRUCT: {
                error("Unimplemented!");
            }
            default: {
                push(parse_funcdecl());
                break;
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
