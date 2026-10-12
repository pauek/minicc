#include "parser.hh"
#include <functional>

#include "astcpp.hh"
#include "tokens.hh"
#include "utils.hh"

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
	constexpr const char *basic_types[] = { "bool", "char", "int", "float", "double" };
	for (auto name : basic_types) {
		if (at_str(name)) {
			advance(1);
			return Identifier::make({ .name = name });
		}
	}
	error("Keyword is not a type!");
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

Ref<FuncDecl> Parser::parse_funcdecl() {
    auto return_type = parse_typespec();
    auto ident = parse_identifier();
    auto params = parse_function_parameters();
    if (not at(SEMICOLON)) {
        error("Expected semicolon here.");
    }
    advance(1);
    return FuncDecl::make({
        .return_type = return_type,
        .ident = ident,
        .params = params,
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
