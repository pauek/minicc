#include "astdump.hh"
#include "astcpp.hh"

void AstDumper::dump(const Include& I) {
    char open = I.global ? '<' : '"';
    char close = I.global ? '>' : '"';
    out_ << "#include " << open << I.filename << close << endl;
}

void AstDumper::dump(const Macro& M) {
    out_ << '#' << M.name;
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
    out_ << "using namespace " << U.namespc << endl;
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
    	const vector<Stmt>& stmts = FD.body.get().stmts;
     	for (auto stmt : stmts) {
      		out_ << "    ";
      		dump(stmt);
      	}
       	out_ << endl;
    }
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
			out_ << expr.as<CharLiteral>().value;
			break;
		}
		case StringLiteral::type_id: {
			out_ << expr.as<StringLiteral>().value;
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
	out_ << "return ";
	dump(retstmt.expr);
	out_ << endl;
}

void AstDumper::dump(const AssignStmt& astmt) {
	dump(astmt.target.get());
	out_ << " = ";
	dump(astmt.expr);
	out_ << endl;
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
		case StmtBlock::type_id: {
			break;
		}
		default:
			assert(false);
	}
}

void AstDumper::dump(const Program& P) {
    for (auto decl : P.declarations) {
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
                dump(decl.as<FuncDecl>());
                break;
            }
        }
    }
}
