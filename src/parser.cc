#include "parser.hh"
#include <assert.h>
#include <cstdlib>
#include <fstream>
#include <sstream>
#include "translator.hh"
using namespace std;

static const char *_basic_types[] = {
    "int",
    "char",
    "string",
    "double",
    "float",
    "short",
    "long",
    "bool",
    "void",
    "vector",
    "list",
    "map",
    "set",
    "pair"
};

Parser::Parser(istream *i, std::ostream *err) : _lexer(i), _err(err) {
    for (int i = 0; i < sizeof(_basic_types) / sizeof(char *); i++) {
        _types.insert(_basic_types[i]);
    }
    if (!_lexer.next()) {
        throw new ParseError(_lexer.pos(), "Can't move to first position");
    }
}

StmtError *Parser::_stmt_error(string msg) {
    auto *s = new StmtError(_lexer.skip_to(";"));
    _error(s, msg);
    return s;
}

AstNode *Parser::parse() {
    auto *prog = new Program();

    _skip(prog);

    while (!_lexer.end()) {
        Pos   pos = _lexer.pos();
        Token tok = _lexer.peek_token();
        switch (tok.type) {
            case Token::Sharp: {
                prog->add(parse_macro(prog));
                break;
            }
            case Token::Using: {
                prog->add(parse_using_declaration(prog));
                break;
            }
            case Token::Struct: {
                StructDecl *decl = parse_struct(prog);
                _types.insert(decl->name);
                prog->add(decl);
                break;
            }
            case Token::Typedef: {
                TypedefDecl *typdef = parse_typedef(prog);
                _types.insert(typdef->decl->name);
                prog->add(typdef);
                break;
            }
            case Token::Enum: {
                EnumDecl *enumdecl = parse_enum(prog);
                _types.insert(enumdecl->name);
                prog->add(enumdecl);
                break;
            }
            case Token::Class: {
                prog->add(_stmt_error(_T("UNIMPLEMENTED")));
                _lexer.skip_to(";");
                break;
            }
            case Token::Empty: {
                ostringstream msg;
                msg << pos << ": " << _T("Unexpected character '%c'", _lexer.curr());
                prog->add(_stmt_error(msg.str()));
                _lexer.read_token();
                break;
            }
            default:
                if (tok.is_ident() or tok.is_type_spec()) {
                    prog->add(parse_func_or_var(prog));
                    break;
                }
                string s = _lexer.substr(tok);
                _error(
                    prog,
                    Span(_lexer.pos()),
                    _T("Unexpected '%s' here.", s.c_str()),
                    {.stopper = true}
                );
                _lexer.read_token();
                break;
        }
        _skip(prog);
    }
    return prog;
}

AstNode *Parser::parse_macro(AstNode *parent) {
    Pos ini = _lexer.pos();

    _lexer.consume(Token::Sharp);
    // comments between '#' and the macro name are gobbled up...
    _lexer.skip(Lexer::Skip::SpaceTab);

    Pos macro_ini = _lexer.pos();
    if (!_lexer.expect("include")) {
        Token  tok = _lexer.read_ident();
        string macro_name = _lexer.substr(tok);
        _lexer.skip_to("\n");
        Pos macro_fin = _lexer.pos();
        _lexer.next();
        auto *m = new Macro(_lexer.substr(macro_ini, macro_fin));
        m->span = Span(ini, macro_fin);
        _fatal_error(macro_fin, _T("Macro '#%s' unknown.", macro_name.c_str()));
        return m;
    }

    auto *inc = new Include();
    _skip(inc);

    char open = _lexer.curr();
    if (open != '"' && open != '<') {
        _error(inc, Span(_lexer.pos()), _T("Expected '\"' or '<' here."));
        _lexer.skip_to("\n");
        return inc;
    }
    char   close = (open == '"' ? '"' : '>');
    bool   is_global = (open == '<');
    string filename;

    _lexer.next();
    while (_lexer.curr() != close) {
        if (_lexer.curr() == '\n') {
            Pos fin = _lexer.pos();
            inc->span = Span(ini, fin);
            _fatal_error(fin, _T("'#include' missing closing '%c'.", close));
            break;
        }
        filename += _lexer.curr();
        Pos p = _lexer.pos();

        _lexer.next();
        if (_lexer.end()) {
            _error(inc, Span(p, _lexer.pos()), _T("'#include' missing closing '%c'", close));
            break;
        }
    }
    if (_lexer.curr() == close) {
        _lexer.next();
    }

    inc->filename = filename;
    inc->global = (close == '>');
    inc->span = Span(ini, _lexer.pos());

    // Catch possible mistake: ending #include with ';'
    auto tok = _lexer.peek_token();
    if (tok.type == Token::SemiColon) {
        _error(inc, Span(_lexer.pos()), _T("Don't end #includes with a semicolon."));
        _lexer.read_token();  // consume it!
    }

    return inc;
}

AstNode *Parser::parse_using_declaration(AstNode *parent) {
    auto *u = new Using();
    Pos   ini = _lexer.pos();
    _lexer.consume("using");

    _skip(u);

    if (!_lexer.expect(Token::Namespace)) {
        _error(u, Span(ini), _T("Expected '%s' here.", "namespace"));
        _lexer.skip_to("\n");
        return u;
    }

    _skip(u);

    Token tok = _lexer.read_ident();
    u->namespc = _lexer.substr(tok);

    _skip(u);

    const Pos fin = _lexer.pos();
    u->span = Span(ini, fin);
    if (!_lexer.expect(Token::SemiColon)) {
        _error(u, Span(fin), _T("Expected '%s' here.", ";"));
    }
    return u;
}

Identifier *Parser::parse_ident(AstNode *parent, Token tok, Pos ini) {
    auto *id = new Identifier(_lexer.substr(tok));
    Pos   fin = _lexer.pos();
    while (true) {
        tok = _lexer.peek_token();
        if (_is_type(id->name) and tok.type == Token::LT) {  // template_id
            _skip(id);

            _lexer.consume("<");
            _skip(id);

            _parse_type_seq(id, id->subtypes);
            _skip(id);

            if (_lexer.curr() != '>') {  // Do NOT call read_token here, since it will return ">>"
                _error(id, Span(_lexer.pos()), _T("Expected '%s' here.", ">"));
            } else {
                _lexer.next();
            }
            fin = _lexer.pos();
        }
        tok = _lexer.peek_token();
        if (tok.type != Token::ColonColon) {
            break;
        }
        _skip(id);

        _lexer.consume("::");
        _skip(id);

        tok = _lexer.read_token();
        if (!tok.is_ident()) {
            _error(id, _T("Expected an identifier here"));
        }
        id->shift(_lexer.substr(tok));
        fin = _lexer.pos();
    }
    id->span = Span(ini, fin);
    return id;
}

bool Parser::_parse_type_process_token(TypeSpec *type, Token tok, Pos p) {
    if (tok.is_type_qual()) {
        switch (tok.type) {
            case Token::Const:
                type->add_qualifier(TypeSpec::Const);
                break;
            case Token::Mutable:
                type->add_qualifier(TypeSpec::Mutable);
                break;
            case Token::Register:
                type->add_qualifier(TypeSpec::Register);
                break;
            case Token::Volatile:
                type->add_qualifier(TypeSpec::Volatile);
                break;
            case Token::Extern:
                type->add_qualifier(TypeSpec::Extern);
                break;
            case Token::Unsigned:
                type->add_qualifier(TypeSpec::Unsigned);
                break;
            case Token::Long:
                type->add_qualifier(TypeSpec::Long);

            default: /* TODO: acabar! */
                break;
        }
        return true;
    } else if (tok.is_basic_type()) {
        if (type->id != 0) {
            _error(type, _T("Basic types are not templates"));
        }
        type->id = new Identifier(_lexer.substr(tok));
        return true;
    } else if (type->id == 0 and tok.is_ident()) {
        type->id = parse_ident(type, tok, p);
        return true;
    } else if (tok.type == Token::Amp) {
        type->reference = true;
        return true;
    } else {
        return false;
    }
}

TypeSpec *Parser::parse_typespec(AstNode *parent) {
    auto *type = new TypeSpec();
    type->parent = parent;

    Pos p = _lexer.pos();
    _lexer.save();

    Token tok = _lexer.read_token();
    while (_parse_type_process_token(type, tok, p)) {
        _lexer.discard();
        _lexer.save();

        _skip(type);

        p = _lexer.pos();
        tok = _lexer.read_token();
    }
    _lexer.restore();
    return type;
}

AstNode *Parser::parse_func_or_var(AstNode *parent) {
    CommentSeq *cseq[2] = {0, 0};
    Pos         ini = _lexer.pos();

    _lexer.save();

    auto *typespec = parse_typespec(0);
    cseq[0] = _lexer.skip();

    Pos   id_ini = _lexer.pos();
    Token tok = _lexer.read_ident();  // should be the function name
    if (tok.type == Token::Unknown) {
        delete typespec;
        _lexer.restore();
        auto stmt = parse_declstmt(parent);
        if (!_lexer.expect(Token::SemiColon)) {
            _error(stmt, Span(_lexer.pos()), _T("Expected '%s' here.", ";"), {.stopper = true});
        }
        return stmt;
    }

    auto *id = parse_ident(0, tok, id_ini);
    cseq[1] = _lexer.skip();

    if (_lexer.curr() == '(') {
        _lexer.discard();
        auto *fn = new FuncDecl(id);
        fn->parent = parent;
        fn->comments.assign(cseq, cseq + 2);
        fn->return_typespec = typespec;
        fn->span.begin = ini;

        id->parent = fn;
        typespec->parent = fn;

        parse_function(fn);
        return fn;
    } else {
        delete typespec;
        _lexer.restore();
        auto stmt = parse_declstmt(parent);
        if (!_lexer.expect(Token::SemiColon)) {
            _error(stmt, Span(_lexer.pos()), _T("Expected '%s' here.", ";"), {.stopper = true});
        }
        return stmt;
    }

    return nullptr;
}

void Parser::parse_function(FuncDecl *fn) {
    CommentSeq *cn;
    // parameter list
    _lexer.consume('(');
    while (true) {
        _skip(fn);

        if (_lexer.curr() == ')') {
            break;
        }
        FuncDecl::Param *p = new FuncDecl::Param(_lexer.pos());
        p->typespec = parse_typespec(fn);

        _skip(fn);

        Token tok = _lexer.read_ident();
        if (tok.type == Token::Unknown) {
            throw ParseError(_lexer.pos(), "Expected identifier");
            // _error(fn, Span(_lexer.pos()), _T("Expected identifier", _lexer.curr()));
            // return;
        }
        p->name = _lexer.substr(tok);

        _skip(fn);

        fn->params.push_back(p);
        p->fin = _lexer.pos();
        if (_lexer.curr() == ')') {
            break;
        } else if (_lexer.curr() == ',') {
            _lexer.consume(',');
        } else {
            _error(
                fn,
                Span(_lexer.pos()),
                _T("Unexpected character '%c' in parameter list", _lexer.curr())
            );
            _lexer.skip_to(")");
        }
    }
    _lexer.consume(')');

    _skip(fn);

    if (_lexer.curr() == ';') {
        fn->block = 0;
        _lexer.next();
    } else {
        fn->block = parse_block(fn);
    }
    fn->span.end = _lexer.pos();

    // Catch possible mistake: ending a function definition with ';'
    auto tok = _lexer.peek_token();
    if (tok.type == Token::SemiColon) {
        _error(fn, Span(_lexer.pos()), _T("Do not end function definitions with a semi-colon."));
        _lexer.read_token();  // consume it!
    }
}

Block *Parser::parse_block(AstNode *parent) {
    auto *block = new Block();
    block->parent = parent;
    Pos ini = _lexer.pos();
    if (!_lexer.expect(Token::LBrace)) {
        _error(block, _T("I expected a '%s' here.", "{"));
        return block;
    }

    _skip(block);

    bool closing_curly = false;
    while (!_lexer.end()) {
        if (_lexer.curr() == '}') {
            closing_curly = true;
            _lexer.next();
            break;
        }
        Stmt *stmt = parse_stmt(block);
        block->stmts.push_back(stmt);

        _skip(block);
    }
    if (!closing_curly) {
        _error(block, Span(_lexer.pos()), _T("Expected '}' but end of text found."));
    }
    block->span = Span(ini, _lexer.pos());

    return block;
}

Stmt *Parser::parse_stmt(AstNode *parent) {
    Stmt *stmt;
    Token tok = _lexer.peek_token();
    switch (tok.type) {
        case Token::LBrace:
            return parse_block(parent);
        case Token::While:
            return parse_while(parent);
        case Token::For:
            return parse_for(parent);
        case Token::If:
            return parse_ifstmt(parent);
        case Token::Switch:
            return parse_switch(parent);

        case Token::LParen: {
            stmt = parse_exprstmt(parent);
            break;
        }
        case Token::Break:
        case Token::Continue:
        case Token::Goto: {
            stmt = parse_jumpstmt(parent);
            break;
        }
        case Token::Return: {
            stmt = parse_exprstmt(parent, true);
            break;
        }
        case Token::Else: {
            throw ParseError(_lexer.pos(), _T("Unexpected '%s' here.", "else"));
        }
        default:
            if (tok.is_operator()) {
                stmt = parse_exprstmt(parent);
            } else {
                stmt = parse_decl_or_expr_stmt(parent);
            }
    }

    _skip(stmt);

    // Expect a semicolon at the end of most statements
    if (!_lexer.expect(Token::SemiColon)) {
        _error(stmt, Span(stmt->span.end), _T("Expected a ';'."));
    }

    return stmt;
}

Stmt *Parser::parse_decl_or_expr_stmt(AstNode *parent) {
    _lexer.save();
    auto *declstmt = parse_declstmt(parent);
    Token tok = _lexer.peek_token();
    if (!has_errors(declstmt) && (tok.type == Token::SemiColon || tok.type == Token::Colon)) {
        _lexer.discard();
        return declstmt;
    }
    delete declstmt;
    _lexer.restore();
    return parse_exprstmt(parent);
}

Stmt *Parser::parse_jumpstmt(AstNode *parent) {
    auto *stmt = new JumpStmt();
    stmt->parent = parent;
    stmt->span.begin = _lexer.pos();

    Token tok = _lexer.read_token();
    switch (tok.type) {
        case Token::Break:
            stmt->kind = JumpStmt::Break;
            break;
        case Token::Continue:
            stmt->kind = JumpStmt::Continue;
            break;
        case Token::Goto:
            stmt->kind = JumpStmt::Goto;
            break;
        default:
            break;
    }

    if (stmt->kind == JumpStmt::Goto) {
        _skip(stmt);

        Token tok = _lexer.read_ident();
        stmt->label = _lexer.substr(tok);
    }

    return stmt;
}

ExprStmt *Parser::parse_exprstmt(AstNode *parent, bool is_return) {
    auto *stmt = new ExprStmt();
    stmt->parent = parent;
    stmt->span.begin = _lexer.pos();
    stmt->is_return = is_return;

    if (is_return) {
        Token tok = _lexer.read_token();
        assert(tok.type == Token::Return);
        _skip(stmt);
    }

    Pos eini = _lexer.pos();
    if (_lexer.curr() == ';') {
        stmt->expr = nullptr;
    } else {
        stmt->expr = parse_expr(stmt);
    }

    Pos efin = _lexer.pos();
    if (stmt->expr) {
        stmt->expr->span = Span(eini, efin);
    }

    stmt->span.end = _lexer.pos();
    return stmt;
}

Expr *Parser::parse_primary_expr(AstNode *parent) {
    Expr *e;

    Pos   ini = _lexer.pos();
    Token tok = _lexer.peek_token();
    switch (tok.type) {
        case Token::LParen: {
            _lexer.read_token();
            CommentSeq *cseq = _lexer.skip();
            e = parse_expr(parent);
            e->paren = true;
            e->comments.insert(e->comments.begin(), cseq);

            _skip(e);

            if (!_lexer.expect(Token::RParen)) {
                _error(e, _lexer.pos().str() + ": Expected ')'");
            }
            e->span = Span(ini, _lexer.pos());
            break;
        }
        case Token::LBrace: {
            // C++11 initializer list
            e = parse_exprlist(parent);
            break;
        }
        case Token::True:
        case Token::False: {
            _lexer.read_token();
            auto *lit = new Literal(Literal::Bool);
            lit->parent = parent;
            lit->val.as_bool = (tok.type == Token::True);
            lit->span = Span(ini, _lexer.pos());

            _skip(lit);

            e = lit;
            break;
        }
        case Token::IntLiteral: {
            _lexer.read_token();
            auto *lit = new Literal(Literal::Int);
            lit->parent = parent;
            lit->val.as_int = atoi(_lexer.substr(tok).c_str());
            lit->span = Span(ini, _lexer.pos());

            _skip(lit);

            e = lit;
            break;
        }
        case Token::CharLiteral: {
            _lexer.read_token();
            auto *lit = new Literal(Literal::Char);
            lit->parent = parent;
            lit->val.as_char = _translate_Escapes(_lexer.substr(tok))[0];
            lit->span = Span(ini, _lexer.pos());

            _skip(lit);

            e = lit;
            break;
        }
        case Token::Dot:
        case Token::FloatLiteral:
        case Token::DoubleLiteral: {
            _lexer.read_token();
            auto *lit =
                new Literal(tok.type == Token::FloatLiteral ? Literal::Float : Literal::Double);

            istringstream S(_lexer.substr(tok));
            S >> lit->val.as_double;
            lit->parent = parent;
            lit->span = Span(ini, _lexer.pos());

            _skip(lit);

            e = lit;
            break;
        }
        case Token::StringLiteral: {
            // Two consecutive string literals are joined together
            auto *lit = new Literal(Literal::String);
            lit->parent = parent;
            lit->span.begin = ini;
            lit->val.as_string.s = new string("");
            string& content = *(lit->val.as_string.s);
            do {
                _lexer.read_token();
                // FIXME: Shouldn't copy string
                content += _translate_Escapes(_lexer.substr(tok));
                lit->span.end = _lexer.pos();

                _skip(lit);

                tok = _lexer.peek_token();
            } while (tok.type == Token::StringLiteral);

            e = lit;
            break;
        }
        default: {
            _lexer.read_token();
            e = parse_ident(parent, tok, ini);
            break;
        }
    }
    return e;
}

string Parser::_translate_Escapes(string s) {
    string result;
    for (int i = 0; i < s.size(); i++) {
        if (s[i] == '\\') {
            i++;
            assert(i < s.size());
            switch (s[i]) {
                case 'a':
                    result += '\a';
                    break;
                case 'b':
                    result += '\b';
                    break;
                case 'f':
                    result += '\f';
                    break;
                case 'n':
                    result += '\n';
                    break;
                case 'r':
                    result += '\r';
                    break;
                case 't':
                    result += '\t';
                    break;
                case 'v':
                    result += '\v';
                    break;
                case '\'':
                    result += '\'';
                    break;
                case '\"':
                    result += '\"';
                    break;
                case '\?':
                    result += '\?';
                    break;
                case '\\':
                    result += '\\';
                    break;
                default:
                    // FIXME: Don't know where to handle this error...
                    cerr << "warning: unknown Escape sequence '\\" << s[i] << "'" << endl;
                    assert(false);
            }
        } else {
            result += s[i];
        }
    }
    return result;
}

Expr *Parser::parse_postfix_expr(AstNode *parent, Expr *e = nullptr) {
    if (e == nullptr) {
        e = parse_primary_expr(parent);
    }
begin:
    Token tok = _lexer.peek_token();
    switch (tok.type) {
        case Token::LParen:
            e = parse_callexpr(e);
            goto begin;
        case Token::LBracket:
            e = parse_indexexpr(e);
            goto begin;
        case Token::Dot:
        case Token::Arrow:
            e = parse_fieldexpr(e, tok);
            goto begin;
        case Token::PlusPlus:
        case Token::MinusMinus:
            e = parse_increxpr(e, tok);
            goto begin;
        default:
            break;
    }
    return e;
}

Expr *Parser::parse_unary_expr(AstNode *parent) {
    Expr *e;
    Pos   ini = _lexer.pos();
    Token tok = _lexer.peek_token();
    switch (tok.type) {
        case Token::Not:
        case Token::Excl: {
            auto *ne = new NegExpr();
            _lexer.read_token();

            _skip(ne);

            ne->expr = parse_unary_expr(ne);
            ne->span = Span(ini, _lexer.pos());
            e = ne;
            break;
        }
        case Token::Plus:
        case Token::Minus: {
            auto *se =
                new SignExpr(tok.type == Token::Plus ? SignExpr::Positive : SignExpr::Negative);
            _lexer.next();

            _skip(se);

            se->expr = parse_unary_expr(se);
            se->span = Span(ini, se->expr->span.end);
            e = se;
            break;
        }
        case Token::Amp: {
            auto *ae = new AddrExpr();
            _lexer.next();

            _skip(ae);

            ae->expr = parse_unary_expr(ae);
            ae->span = Span(ini, ae->expr->span.end);
            e = ae;
            break;
        }
        case Token::Star: {
            auto *de = new DerefExpr();
            _lexer.next();

            _skip(de);

            de->expr = parse_unary_expr(de);
            de->span = Span(ini, de->expr->span.end);
            e = de;
            break;
        }
        case Token::MinusMinus:
        case Token::PlusPlus: {
            auto *ie = new IncrExpr();
            ie->kind = tok.type == Token::PlusPlus ? IncrExpr::Positive : IncrExpr::Negative;
            _lexer.consume(tok.type == Token::PlusPlus ? "++" : "--");
            CommentSeq *comm = _lexer.skip();
            ie->expr = parse_unary_expr(ie);
            ie->preincr = true;
            ie->span = Span(ini, ie->expr->span.end);
            ie->comments.insert(ie->comments.begin(), comm);
            e = ie;
            break;
        }
        default:
            e = parse_postfix_expr(parent);
            break;
    }
    return e;
}

Expr *Parser::parse_expr(AstNode *parent, BinaryExpr::Kind max) {
    Expr *left = parse_unary_expr(parent);
    while (true) {
        Token tok = _lexer.peek_token();
        if (!tok.is_operator()) {
            break;
        }
        BinaryExpr::Kind kind = BinaryExpr::token_to_kind(tok.type);
        if (tok.type == Token::Empty or kind > max) {
            break;
        }
        CommentSeq *c0 = _lexer.skip();
        tok = _lexer.read_token();
        if (!tok.is_operator()) {
            _error(left, _T("Expected operator here."));
        }
        if (tok.type == Token::QMark) {  // (... ? ... : ...)
            auto *e = new CondExpr();
            e->cond = left;
            e->comments.push_back(c0);

            _skip(e);

            e->then = parse_expr(e, Expr::Eq);  // Expr::comma?

            _skip(e);

            Token colon = _lexer.read_token();
            if (colon.type != Token::Colon) {
                _error(e, Span(_lexer.pos()), _T("Expected '%s' here.", ":"));
            }

            _skip(e);

            e->els = parse_expr(e, Expr::Eq);
            left = e;
        } else {
            auto *e = new BinaryExpr();
            e->op = _lexer.substr(tok);
            e->kind = kind;
            e->comments.push_back(c0);

            _skip(e);

            Expr::Kind submax = Expr::Kind(Expr::right_associative(kind) ? kind : kind - 1);
            Expr      *right = parse_expr(e, submax);
            e->left = left;
            e->right = right;
            e->span = Span(left->span.begin, right->span.end);
            left = e;
        }
    }
    return left;
}

Expr *Parser::parse_callexpr(Expr *x) {
    auto *e = new CallExpr();
    e->func = x;
    e->parent = x->parent;
    x->parent = e;

    _skip(e);

    _lexer.consume('(');

    _skip(e);

    if (_lexer.curr() != ')') {
        e->args.push_back(parse_expr(e, Expr::Eq));

        _skip(e);
        while (_lexer.curr() == ',') {
            _lexer.next();
            _skip(e);
            e->args.push_back(parse_expr(e, Expr::Eq));
            _skip(e);
        }
    }
    if (!_lexer.expect(Token::RParen)) {
        _error(e, Span(_lexer.pos()), _T("Expected '%s' here.", ")"));
    }
    e->span = Span(x->span.begin, _lexer.pos());
    return e;
}

Expr *Parser::parse_indexexpr(Expr *x) {
    auto *e = new IndexExpr();
    e->base = x;

    _skip(e);

    _lexer.consume('[');
    if (_lexer.curr() != ']') {
        _skip(e);
        e->index = parse_expr(e);
        _skip(e);
        if (!_lexer.expect(Token::RBracket)) {
            _error(e, Span(_lexer.pos()), _T("Expected '%s' here.", "]"));
        }
    } else {
        _fatal_error(_lexer.pos(), _T("Debe haber una expresión entre los corchetes."));
        _lexer.consume(']');
    }
    e->span = Span(x->span.begin, _lexer.pos());

    _skip(e);
    return e;
}

Expr *Parser::parse_fieldexpr(Expr *x, Token tok) {
    auto *e = new FieldExpr();
    e->base = x;
    e->pointer = (tok.type == Token::Arrow);
    _lexer.consume(tok.type == Token::Arrow ? "->" : ".");

    _skip(e);

    Token id = _lexer.read_ident();
    e->field = _lexer.substr(id);
    e->span = Span(x->span.begin, _lexer.pos());
    return e;
}

Expr *Parser::parse_increxpr(Expr *x, Token tok) {
    auto *e = new IncrExpr();
    e->kind = tok.type == Token::PlusPlus ? IncrExpr::Positive : IncrExpr::Negative;
    e->expr = x;

    _skip(e);

    _lexer.consume(tok.type == Token::PlusPlus ? "++" : "--");
    e->span = Span(x->span.begin, _lexer.pos());
    return e;
}

bool wrong_for_with_commas(string code) {
    vector<int> commas, colons;
    for (int i = 0; i < code.size(); i++) {
        if (code[i] == ',') {
            commas.push_back(i);
        } else if (code[i] == ';') {
            colons.push_back(i);
        }
    }
    return commas.size() == 2 and colons.size() == 0;
}

Stmt *Parser::parse_for(AstNode *parent) {
    _lexer.consume("for");

    CommentSeq *c0 = _lexer.skip();

    if (!_lexer.expect(Token::LParen)) {
        throw ParseError(_lexer.pos(), _T("Expected '%s' here.", "("));
    }

    CommentSeq *c1 = _lexer.skip();

    if (_lexer.curr() == ';') {
        // First slot in the 'for' is empty
        auto *stmt = new ForStmt();
        stmt->parent = parent;
        stmt->comments.push_back(c0);
        stmt->comments.push_back(c1);
        _lexer.next();

        stmt->init = nullptr;
        return _parse_for_classic(stmt);
    } else {
        auto *stmt = new ForStmt();
        stmt->parent = parent;
        stmt->comments.push_back(c0);
        stmt->comments.push_back(c1);

        Stmt *decl = parse_decl_or_expr_stmt(stmt);
        Token tok = _lexer.peek_token();
        switch (tok.type) {
            case Token::SemiColon:
                stmt->init = decl;
                _lexer.read_token();  // consume ';'
                break;
            case Token::Colon: {
                _lexer.read_token();  // consume ':'
                return _parse_for_colon(decl);
            }
            default:
                _error(stmt, Span(_lexer.pos()), _T("Expected ';' or ':' here"));
                return _parse_for_classic(stmt);
        }
        return _parse_for_classic(stmt);
    }
}

ForColonStmt *Parser::_parse_for_colon(Stmt *decl) {
    ForColonStmt *stmt = new ForColonStmt();
    stmt->decl = decl;

    _skip(stmt);

    Expr *container_expr = parse_expr(stmt);
    stmt->container = container_expr;

    _skip(stmt);

    if (!_lexer.expect(Token::RParen)) {
        _error(stmt, Span(_lexer.pos()), _T("Expected '%s' here.", ")"));
    }

    _skip(stmt);

    stmt->substmt = parse_stmt(stmt);
    return stmt;
}

ForStmt *Parser::_parse_for_classic(ForStmt *stmt) {
    _skip(stmt);

    if (_lexer.curr() == ';') {
        _lexer.next();
        stmt->cond = 0;
    } else {
        stmt->cond = parse_expr(stmt);
    }

    _skip(stmt);

    if (!_lexer.expect(Token::SemiColon)) {
        _error(stmt, Span(_lexer.pos()), _T("Expected '%s' here.", ";"));
    }

    _skip(stmt);

    if (_lexer.curr() == ')') {
        stmt->post = nullptr;
    } else {
        stmt->post = parse_expr(stmt);
    }

    _skip(stmt);

    if (!_lexer.expect(Token::RParen)) {
        _error(stmt, Span(_lexer.pos()), _T("Expected '%s' here.", ")"));
    }

    _skip(stmt);

    stmt->substmt = parse_stmt(stmt);
    return stmt;
}

Stmt *Parser::parse_while(AstNode *parent) {
    auto *stmt = new WhileStmt();
    stmt->parent = parent;
    _lexer.consume("while");

    _skip(stmt);

    if (!_lexer.expect(Token::LParen)) {
        _error(stmt, Span(_lexer.pos()), _T("Expected '%s' here.", "("));
    }

    _skip(stmt);

    stmt->cond = parse_expr(stmt);

    _skip(stmt);

    if (!_lexer.expect(Token::RParen)) {
        throw ParseError(_lexer.pos(), _T("Expected '%s' here.", ")"));
    }

    _skip(stmt);

    stmt->substmt = parse_stmt(stmt);
    return stmt;
}

Stmt *Parser::parse_ifstmt(AstNode *parent) {
    auto *stmt = new IfStmt();
    stmt->parent = parent;
    _lexer.consume("if");

    _skip(stmt);

    if (!_lexer.expect(Token::LParen)) {
        _error(stmt, Span(_lexer.pos()), _T("Expected '%s' here.", "("));
    }

    _skip(stmt);

    stmt->cond = parse_expr(stmt);

    _skip(stmt);

    if (!_lexer.expect(Token::RParen)) {
        _error(stmt, Span(_lexer.pos()), _T("Expected '%s' here.", ")"));
    }

    _skip(stmt);

    stmt->then = parse_stmt(stmt);
    _lexer.save();

    _skip(stmt);

    string tok;
    if (_lexer.peek_token().type == Token::Else) {
        _lexer.expect(Token::Else);
        _lexer.discard();

        _skip(stmt);

        stmt->els = parse_stmt(stmt);
    } else {
        stmt->comments.pop_back();
        _lexer.restore();
    }
    return stmt;
}

Stmt *Parser::parse_switch(AstNode *parent) {
    return _stmt_error(_T("UNIMPLEMENTED"));
}

void Parser::_parse_expr_seq(AstNode *parent, vector<Expr *>& exprs) {
    exprs.push_back(parse_expr(parent, Expr::Eq));
    while (_lexer.curr() == ',') {
        _lexer.next();
        _skip(parent);
        exprs.push_back(parse_expr(parent, Expr::Eq));
    }
}

void Parser::_parse_type_seq(AstNode *parent, vector<TypeSpec *>& seq) {
    seq.push_back(parse_typespec(parent));
    _skip(parent);
    while (_lexer.curr() == ',') {
        _lexer.next();
        _skip(parent);
        seq.push_back(parse_typespec(parent));
    }
}

Expr *Parser::parse_exprlist(AstNode *parent) {
    assert(_lexer.curr() == '{');
    auto *elist = new ExprList();
    do {
        _lexer.next();
        _skip(elist);
        if (_lexer.curr() == '}') {
            break;
        }
        elist->exprs.push_back(
            _lexer.curr() == '{' ? parse_exprlist(parent) : parse_expr(parent, Expr::Eq)
        );
    } while (_lexer.curr() == ',');
    if (!_lexer.expect(Token::RBrace)) {
        _error(elist, Span(_lexer.pos()), _T("Expected '%s' here.", "}"));
        _lexer.skip_to("},;\n");
    }
    _skip(elist);
    return elist;
}

Decl *Parser::_parse_vardecl(AstNode *parent, string name, Decl::Kind kind, CommentSeq *comm) {
    auto *decl = new VarDecl(name);
    decl->parent = parent;
    decl->kind = kind;
    decl->comments.push_back(comm);
    return decl;
}

Decl *Parser::_parse_arraydecl(AstNode *parent, string name, Decl::Kind kind, CommentSeq *comm) {
    auto *decl = new ArrayDecl();
    decl->comments.push_back(comm);
    decl->parent = parent;
    decl->name = name;
    decl->kind = kind;

    while (_lexer.curr() == '[') {
        _lexer.consume("[");
        _skip(decl);
        decl->sizes.push_back(parse_expr(decl, Expr::Conditional));
        if (!_lexer.expect(Token::RBracket)) {
            _error(decl, Span(_lexer.pos()), _T("Expected '%s' here.", "]"));
        }
        _skip(decl);
    }

    return decl;
}

Decl *Parser::_parse_objdecl(AstNode *parent, string name, CommentSeq *comm) {
    _lexer.consume("(");

    auto *decl = new ObjDecl();
    decl->comments.push_back(comm);
    decl->parent = parent;
    decl->name = name;

    _skip(decl);

    _parse_expr_seq(decl, decl->args);
    if (!_lexer.expect(Token::RParen)) {
        _error(decl, Span(_lexer.pos()), _T("Expected '%s' here.", ")"));
        _lexer.skip_to("),;\n");
    }

    return decl;
}

DeclStmt *Parser::parse_declstmt(AstNode *parent, bool is_typedef) {
    auto *stmt = new DeclStmt();
    stmt->parent = parent;
    stmt->span.begin = _lexer.pos();
    TypeSpec *typespec = parse_typespec(stmt);
    stmt->typespec = typespec;

    _skip(stmt);  // before identifier

    Pos after_comma = _lexer.pos(), after_id = _lexer.pos();
    while (true) {
        Pos        item_ini = _lexer.pos();
        Token      id = _lexer.read_token();
        string     name = _lexer.substr(id);
        Decl::Kind kind = Decl::Normal;
        if (id.type == Token::Star) {
            kind = Decl::Pointer;
            _skip(stmt);

            id = _lexer.read_token();
            name = _lexer.substr(id);
        }
        if (!id.is_ident()) {
            _error(stmt, _T("Expected a variable name here."), {.stopper = true});
        }
        after_id = _lexer.pos();
        DeclStmt::Item item;
        CommentSeq    *comm = _lexer.skip();
        if (_lexer.curr() == '(' and !is_typedef) {
            item.decl = _parse_objdecl(stmt, name, comm);
        } else if (_lexer.curr() == '[') {
            item.decl = _parse_arraydecl(stmt, name, kind, comm);
        } else {
            item.decl = _parse_vardecl(stmt, name, kind, comm);
        }
        item.decl->span.begin = item_ini;
        if (_lexer.curr() == '=') {
            // Normal assignment of initial value
            _lexer.next();
            _skip(stmt);
            if (_lexer.curr() == '{') {
                item.init = parse_exprlist(item.decl);
            } else {
                item.init = parse_expr(item.decl, Expr::Eq);
            }
        } else if (_lexer.curr() == '{') {
            // Initializer list without '=', since C++11
            item.init = parse_exprlist(item.decl);
        }
        item.decl->typespec = stmt->typespec;
        item.decl->span.end = _lexer.pos();
        stmt->items.push_back(item);
        if (_lexer.curr() != ',' or is_typedef) {
            break;
        }
        _lexer.consume(",");
        after_comma = _lexer.pos();

        _skip(stmt);  // before identifier
    }
    stmt->span.end = _lexer.pos();

    return stmt;
}

EnumDecl *Parser::parse_enum(AstNode *parent) {
    _lexer.consume("enum");

    auto *decl = new EnumDecl();
    decl->parent = parent;

    _skip(decl);

    Token tok = _lexer.read_token();
    if (!tok.is_ident()) {
        _error(decl, Span(_lexer.pos()), _T("Expected an identifier here."));
        _lexer.skip_to(";");
        return decl;
    }
    decl->name = _lexer.substr(tok);

    _skip(decl);

    if (!_lexer.expect(Token::LBrace)) {
        _error(decl, Span(_lexer.pos()), _T("Expected '%s' here.", "{"));
        _lexer.skip_to("};");
    }

    _skip(decl);

    while (true) {
        Token tok = _lexer.read_token();
        if (!tok.is_ident()) {
            _error(decl, Span(_lexer.pos()), _T("Expected an identifier here."));
            _lexer.skip_to(",}");
            break;
        }
        EnumDecl::Value v(_lexer.substr(tok));

        _skip(decl);

        if (_lexer.curr() == '=') {
            _lexer.next();
            _skip(decl);
            Token num = _lexer.read_number_literal();
            if (num.type != Token::IntLiteral) {
                _error(decl, Span(_lexer.pos()), _T("Expected an integer here."));
                _lexer.skip_to(",};");
            }
            v.has_val = true;
            istringstream S(_lexer.substr(num));
            S >> v.val;

            _skip(decl);
        }
        decl->values.push_back(v);
        if (_lexer.curr() == ',') {
            _lexer.next();

            _skip(decl);
        } else {
            break;
        }
    }
    if (!_lexer.expect(Token::RBrace)) {
        _error(decl, Span(_lexer.pos()), _T("Expected '%s' here.", "{"));
    }
    if (!_lexer.expect(Token::SemiColon)) {
        _error(decl, Span(_lexer.pos()), _T("Expected '%s' here.", ";"));
    }
    return decl;
}

TypedefDecl *Parser::parse_typedef(AstNode *parent) {
    _lexer.consume("typedef");
    auto *typdef = new TypedefDecl();
    typdef->parent = parent;

    _skip(typdef);

    DeclStmt *stmt = parse_declstmt(typdef, true);  // FIXME: Y si hay varias declaraciones??
    if (!_lexer.expect(Token::SemiColon)) {
        _error(stmt, Span(_lexer.pos()), _T("Expected '%s' here.", ";"), {.stopper = true});
    }

    typdef->decl = stmt->items[0].decl;
    for (CommentSeq *c : stmt->comments) {
        typdef->comments.push_back(c);
    }
    delete stmt;
    return typdef;
}

StructDecl *Parser::parse_struct(AstNode *parent) {
    Token tok = _lexer.read_token();
    assert(tok.type == Token::Struct);
    auto *decl = new StructDecl();
    decl->parent = parent;

    _skip(decl);

    Token id = _lexer.read_ident();
    decl->name = _lexer.substr(id);

    _skip(decl);

    tok = _lexer.read_token();
    if (tok.type != Token::LBrace) {
        _error(decl, Span(_lexer.pos()), _T("Expected '%s' here.", "{"));
        _lexer.skip_to("};");
        return decl;
    }

    _skip(decl);

    tok = _lexer.peek_token();
    while (!_lexer.end() and tok.type != Token::RBrace) {
        DeclStmt *field = parse_declstmt(decl);
        if (!_lexer.expect(Token::SemiColon)) {
            _error(field, Span(_lexer.pos()), _T("Expected '%s' here.", ";"), {.stopper = true});
        }
        decl->decls.push_back(field);
        field->parent = decl;
        _skip(decl);
        tok = _lexer.peek_token();
    }
    if (tok.type != Token::RBrace) {
        _error(decl, Span(_lexer.pos()), _T("Expected '%s' here.", "}"));
    }
    _lexer.expect(Token::RBrace);

    _skip(decl);

    if (!_lexer.expect(Token::SemiColon)) {
        _error(decl, Span(_lexer.pos()), _T("Expected '%s' here.", ";"));
    }
    return decl;
}
