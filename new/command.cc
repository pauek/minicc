#include <filesystem>
#include <iostream>

#include "astdump.hh"
using namespace std;

#include "command.hh"
#include "lexer.hh"
#include "parser.hh"
#include "printer.hh"
#include "utils.hh"

int Help(Args _) {
    constexpr const char *usage =
        "Usage: minicc [options...] [command]\n"
        "\n"
        "Simplified C++ compiler and interpreter for educational purposes.\n"
        "\n"
        "Commands:\n"
        "   lex          Lex a file\n"
        "   parse        Parse a file\n"
        "\n";

    cout << usage;
    return 1;
}

int HelpLex() {
    constexpr const char *lex_usage =
        "Usage: minicc lex [options...] [file]...\n"
        "\n"
        "Lists C++ tokens in file(s).\n"
        "\n"
        "Options:\n"
        "    none for now... ;)\n"
        "\n";

    cout << lex_usage;
    return 1;
}

int Lex(Args args) {
    if (args.empty()) {
        return HelpLex();
    }
    for (auto filename : args) {
        string text = read_file(filename);
        if (args.size() > 1) {
            cout << filename << ":" << endl;
        }
        try {
            Lexer lexer(text, filename);
            lexer.run().dump(cout);
        } catch (LexError *e) {
            e->print(cerr);
            return 1;
        }
    }
    return 0;
}

int HelpParse() {
    constexpr const char *parse_usage =
        "Usage: minicc parse [options...] [file]...\n"
        "\n"
        "Shows the AST from file(s).\n"
        "\n"
        "Options:\n"
        "    none for now... ;)\n"
        "\n";

    cout << parse_usage;
    return 1;
}

template <>
ICommand *parse_args<Parse>(Args args) {
    // We still don't have subcommands, just the same base implementation.
    return new Command<Parse>(args);
}

int Parse(Args args) {
    if (args.empty()) {
        return HelpParse();
    }
    for (auto filename : args) {
        // Read file
        string text;
        try {
            text = read_file(filename);
        } catch (filesystem::filesystem_error& e) {
            cerr << "Could not read file '" << filename << "': " << e.what() << endl;
        }
        if (args.size() > 1) {
            cout << filename << ":" << endl;
        }

        try {
            // Lexer pass
            Lexer lexer(text, filename);
            auto  tokens = lexer.run();

            // Parser pass
            Parser parser(tokens, filename);
            auto   program = parser.run();

            // Show result
            AstDumper ast_dumper(cout, tokens);
            ast_dumper.dump(program.get());

        } catch (Error *e) {
            e->print(cerr);
            return 1;
        }
    }
    return 0;
}
