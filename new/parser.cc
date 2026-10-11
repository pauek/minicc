#include "parser.hh"

#include "astcpp.hh"
#include "tokens.hh"

Ref<Macro> Parser::parse_macro() {
    string macro_name = tokens_.str(pos_).substr(1);
    return Macro::make({.macro = macro_name});
}

Ref<Include> Parser::parse_include() {
    assert(at(MACRO) && curr_str() == "#include");
    advance(1);

    string filename;
    bool   global = false;

    if (at(IDENT)) {
        filename = curr_str();
        advance(1);
    } else {
        global = true;
        if (not at(LT)) {
            error("Expected '<' or '\"' after '#include'.");
        }
        advance(1);  // Consume '<'
        if (not at(IDENT)) {
            error("Expected identifier after '<'");
        }
        filename = curr_str();
        advance(1);
        if (not at(GT)) {
            error("Expected '>' at end of '#include'");
        }
        advance(1);
    }
    return Include::make({.filename = filename, .global = global});
}

Ref<Using> Parser::parse_using() {
    assert(at(IDENT) && curr_str() == "using");
    advance(1);

    if (not at(IDENT) || curr_str() != "namespace") {
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

Ref<TypeSpec> Parser::parse_typespec() {
    assert(at(IDENT));
    if (curr_str() == "int") {
        advance(1);
        auto id = Identifier::make({.name = "int"});
        return TypeSpec::make({.ident = id});
    }
    string id = curr_str();
    error("Unexpected token '{}'.", id);
    return TypeSpec::make({
        .ident = Identifier::make({.name = "ERROR"}),
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
                if (curr_str() == "#include") {
                    push(parse_include());
                } else if (at(MACRO)) {
                    push(parse_macro());
                }
                break;
            }
            case IDENT: {
                string id = curr_str();

                // Using declaration
                if (id == "using") {
                    push(parse_using());
                    break;
                }

                // Try typespec
                save();
                auto typespec = parse_typespec();

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
