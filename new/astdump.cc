#include "astdump.hh"
#include <any>

void AstDumper::dump(const Include& include) {
    char open = include.global ? '<' : '"';
    char close = include.global ? '>' : '"';
    out_ << "#include " << open << include.filename << close << endl;
}

void AstDumper::dump(const Macro& macro) {
    out_ << '#' << macro.name;
    for (size_t i = macro.start; i < macro.end; i++) {
        // Just skip ESC and ENDL since they were parsed already
        TokenID tok_id = tokens_.ids[i];
        if (tok_id != ESC && tok_id != ENDL) {
            out_ << ' ' << tokens_.str(i);
        }
    }
    out_ << endl;
}

void AstDumper::dump(const Using& usin) {
    out_ << "using namespace " << usin.namespc << endl;
}

void AstDumper::dump(const TypeSpec& typespec) {
    // Qualifiers
    constexpr TypeSpec::Qualifier qualifiers[] = { TypeSpec::Auto, TypeSpec::Const };
    bool any_qualifiers = false;
    for (auto qual : qualifiers) {
    	if (typespec.bqual & qual) {
       		out_ << TypeSpec::qualifier_to_str(qual) << " ";
     	}
    }

    // Main symbol
    dump(typespec.ident.get());

    // Is it a reference?
    if (typespec.reference) {
    	out_ << " &";
    }
}

void AstDumper::dump(const Identifier& ident) {
    out_ << ident.name;
}

void AstDumper::dump(const FuncParam& param) {
    auto& [type, ident] = param;
    dump(ident.get());
    out_ << ": ";
    dump(type.get());
}

void AstDumper::dump(const FuncDecl& funcdecl) {
    auto& [return_type, ident, params] = funcdecl;
    out_ << "function \"";
    dump(ident.get());
    out_ << "\"(";
    for (size_t i = 0; i < params.size(); i++) {
        if (i > 0) {
            out_ << ", ";
        }
        dump(params[i].get());
    }
    out_ << "): ";
    dump(return_type.get());
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
