#include "astdump.hh"

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
        }
    }
}
