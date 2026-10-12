#include "astdump.hh"

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
