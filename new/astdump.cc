#include "astdump.hh"

#include "astcpp.hh"

void AstDumper::dump(const Include& I) {
    char open = I.global ? '<' : '"';
    char close = I.global ? '>' : '"';
    out_ << indent() << "#include " << open << I.filename << close << endl;
}

void AstDumper::dump(const Macro& M) {
    out_ << indent() << '#' << M.name;
    for (size_t i = M.start; i < M.end; i++) {
        // Just skip ESC and ENDL since they were parsed already
        TokenID tok_id = tokens_.ids[i];
        if (tok_id != ESC && tok_id != ENDL) {
            out_ << ' ' << tokens_.str(i);
        }
    }
    out_ << endl;
}

void AstDumper::dump(const Using& U) {
    out_ << indent() << "using namespace " << U.namespc << endl;
}

constexpr TypeSpec::Qualifier searched_qualifiers[] = {TypeSpec::Auto, TypeSpec::Const};

void AstDumper::dump(const TypeSpec& T) {
    out_ << indent();

    // Qualifiers
    for (auto qual : searched_qualifiers) {
        if (T.bqual & qual) {
            out_ << TypeSpec::qualifier_to_str(qual) << " ";
        }
    }

    // Main symbol
    dump(T.ident.get());

    // Is it a reference?
    if (T.reference) {
        out_ << " &";
    }
}

void AstDumper::dump(const Identifier& I) {
    out_ << I.name;
}

void AstDumper::dump(const FuncParam& FP) {
    auto& [type, ident] = FP;
    dump(ident.get());
    out_ << ": ";
    dump(type.get());
}

void AstDumper::dump(const FuncDecl& FD) {
    assert(indent_ == 0);
    out_ << "function \"";
    dump(FD.ident.get());
    out_ << "\"(";
    for (size_t i = 0; i < FD.params.size(); i++) {
        if (i > 0) {
            out_ << ", ";
        }
        dump(FD.params[i].get());
    }
    out_ << "): ";
    dump(FD.return_type.get());
    out_ << endl;

    // Dump the body, if it exists
    if (not FD.body.empty()) {
        dump(FD.body.get());
    }
}

void AstDumper::dump(const StmtBlock& block) {
    indent(+1);
    const vector<Stmt>& stmts = block.stmts;
    for (auto stmt : stmts) {
        dump(stmt);
    }
    indent(-1);
}

void AstDumper::dump(const BinaryExpr& binexpr) {
    dump(binexpr.left);
    out_ << ' ' << BinaryExpr::operator_to_str(binexpr.op) << ' ';
    dump(binexpr.right);
}

void AstDumper::dump(Expr expr) {
    switch (expr.type_id) {
        case Identifier::type_id: {
            dump(expr.as<Identifier>());
            break;
        }
        case IntLiteral::type_id: {
            out_ << expr.as<IntLiteral>().value;
            break;
        }
        case CharLiteral::type_id: {
            out_ << '\'' << expr.as<CharLiteral>().value << '\'';
            break;
        }
        case StringLiteral::type_id: {
            out_ << expr.as<StringLiteral>().value;
            break;
        }
        case BoolLiteral::type_id: {
            out_ << (expr.as<BoolLiteral>().value ? "true" : "false");
            break;
        }
        case BinaryExpr::type_id: {
            out_ << '(';
            dump(expr.as<BinaryExpr>());
            out_ << ')';
            break;
        }
        default: {
            assert(false);
        }
    }
}

void AstDumper::dump(const ReturnStmt& retstmt) {
    out_ << indent() << "return ";
    dump(retstmt.expr);
    out_ << endl;
}

void AstDumper::dump(const AssignStmt& astmt) {
    out_ << indent();
    dump(astmt.target.get());
    out_ << " = ";
    dump(astmt.expr);
    out_ << endl;
}

void AstDumper::dump(const IfStmt& if_stmt) {
    out_ << indent() << "if ";
    dump(if_stmt.cond);
    out_ << ":" << endl;
    dump(if_stmt.if_true.get());
    if (not if_stmt.if_false.empty()) {
        out_ << indent() << "else:" << endl;
        dump(if_stmt.if_false.get());
    }
}

void AstDumper::dump(Stmt stmt) {
    switch (stmt.type_id) {
        case ReturnStmt::type_id: {
            dump(stmt.as<ReturnStmt>());
            break;
        }
        case AssignStmt::type_id: {
            dump(stmt.as<AssignStmt>());
            break;
        }
        case IfStmt::type_id: {
            dump(stmt.as<IfStmt>());
            break;
        }
        default:
            assert(false);
    }
}

void AstDumper::dump(const Program& P) {
    for (size_t i = 0; i < P.declarations.size(); i++) {
        const TopDecl& decl = P.declarations[i];
        switch (decl.type_id) {
            case Include::type_id: {
                dump(decl.as<Include>());
                break;
            }
            case Macro::type_id: {
                dump(decl.as<Macro>());
                break;
            }
            case Using::type_id: {
                dump(decl.as<Using>());
                break;
            }
            case FuncDecl::type_id: {
                if (i > 0) {
                    out_ << endl;
                }
                dump(decl.as<FuncDecl>());
                break;
            }
        }
    }
}
