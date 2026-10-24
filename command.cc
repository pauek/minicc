#include <filesystem>
#include <fstream>
#include <iostream>
using namespace std;

#include "astdump.hh"
#include "command.hh"
#include "config.hh"
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
        "   config       Show config\n"
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
            lexer.run();
            lexer.tokens().dump(cout);
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
            lexer.run();

            // Parser pass
            Parser parser(lexer);
            auto   program = parser.run();

            // Show result
            AstDumper ast_dumper(cout, lexer.tokens());
            ast_dumper.dump(program.get());

        } catch (Error *e) {
            e->print(cerr);
            return 1;
        }
    }
    return 0;
}

int ShowConfig(Args args) {
    Config::read_from_file();

    if (args.empty()) {
        Config::show();
    } else if (args.size() == 1 && (args[0] == "--show-all" || args[0] == "-a")) {
        Config::show_all_options();
    } else {
        cerr << "Unknown args:";
        for (auto arg : args) {
            cerr << " '" << arg << "'";
        }
        cerr << endl;
        return 1;
    }

    return 0;
}
