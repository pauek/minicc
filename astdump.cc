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
        TokenID tok_id = parser_.tokens().ids[i];
        if (tok_id != ESC && tok_id != ENDL) {
            out_ << ' ' << parser_.tokens().str(i);
        }
    }
    out_ << endl;
}

void AstDumper::dump(const Using& U) {
    out_ << indent() << "using namespace " << U.namespc << endl;
}

constexpr TypeSpec::Qualifier searched_qualifiers[] = {TypeSpec::Auto, TypeSpec::Const};

void AstDumper::dump(const TypeSpec& T) {
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

void AstDumper::dump(const ReturnStmt& stmt) {
    out_ << indent() << "return ";
    dump(stmt.expr);
    out_ << endl;
}

void AstDumper::dump(const AssignStmt& stmt) {
    out_ << indent();
    dump(stmt.assign.get());
    out_ << endl;
}

void AstDumper::dump(const IncrStmt& stmt) {
    out_ << indent();
    auto expr = stmt.incr.get();
    dump(expr.target.get());
    switch (expr.op) {
        case IncrExpr::Add: {
            out_ << "++";
            break;
        }
        case IncrExpr::Sub: {
            out_ << "--";
            break;
        }
        default:
            assert(false);
    }
    out_ << endl;
}

void AstDumper::dump(const CondExpr& expr) {
	dump(expr.cond);
	out_ << ":" << endl;
	dump(expr.then.get());
}

void AstDumper::dump(const IfStmt& stmt) {
    out_ << indent() << "if ";
    assert(not stmt.cond_exprs.empty());
    dump(stmt.cond_exprs[0].get());
    for (size_t i = 1; i < stmt.cond_exprs.size(); i++) {
     	out_ << indent() << "elif ";
    	dump(stmt.cond_exprs[i].get());
    }
    if (not stmt.els.empty()) {
    	out_ << indent() << "else:" << endl;
     	dump(stmt.els.get());
    }
}

void AstDumper::dump(const ForInitStmt& stmt) {
    switch (stmt.type_id) {
        case ForInitDecl::type_id: {
            dump(stmt.as<ForInitDecl>());
            break;
        }
        case ForInitAssign::type_id: {
            dump(stmt.as<ForInitAssign>());
            break;
        }
        default:
            assert(false);
    }
}

void AstDumper::dump(const AssignExpr& assign) {
    dump(assign.target.get());
    out_ << ' ' << AssignExpr::op_to_str(assign.op) << ' ';
    dump(assign.expr);
}

void AstDumper::dump(const ForInitDecl& decl) {
    dump(decl.type.get());
    out_ << ' ';
    for (size_t i = 0; i < decl.variables.size(); i++) {
        if (i > 0) {
            out_ << ',';
        }
        dump(decl.variables[i].get());
    }
}

void AstDumper::dump(const ForInitAssign& init) {
    dump(init.assign.get());
}

void AstDumper::dump(const IncrExpr& expr) {
    const string op = IncrExpr::op_to_str(expr.op);
    if (expr.order == IncrExpr::Pre) {
        out_ << op;
    }
    dump(expr.target.get());
    if (expr.order == IncrExpr::Post) {
        out_ << op;
    }
}

void AstDumper::dump(const ForIncrExpr& for_incr) {
    switch (for_incr.type_id) {
        case AssignExpr::type_id: {
            dump(for_incr.as<AssignExpr>());
            break;
        }
        case IncrExpr::type_id: {
            dump(for_incr.as<IncrExpr>());
            break;
        }
    }
}

void AstDumper::dump(const ForStmt& stmt) {
    out_ << indent() << "for (";
    dump(stmt.init);
    out_ << "; ";
    dump(stmt.cond);
    out_ << "; ";
    dump(stmt.incr);
    out_ << "):" << endl;
    dump(stmt.body.get());
}

void AstDumper::dump(const WhileStmt& stmt) {
    out_ << indent() << "while ";
    dump(stmt.cond);
    out_ << ":" << endl;
    dump(stmt.body.get());
}

void AstDumper::dump(Stmt stmt) {
    switch (stmt.type_id) {
        case ReturnStmt::type_id: {
            dump(stmt.as<ReturnStmt>());
            break;
        }
        case IncrStmt::type_id: {
            dump(stmt.as<IncrStmt>());
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
        case WhileStmt::type_id: {
            dump(stmt.as<WhileStmt>());
            break;
        }
        case ForStmt::type_id: {
            dump(stmt.as<ForStmt>());
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
