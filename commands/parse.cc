#include <filesystem>
#include <iostream>
using namespace std;

#include "../astdump.hh"
#include "../lexer.hh"
#include "../parser.hh"
#include "../printer.hh"
#include "../utils.hh"
#include "command.hh"

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
            AstDumper ast_dumper(cout, parser);
            ast_dumper.dump(program.get());

        } catch (Error *e) {
            e->print(cerr);
            return 1;
        }
    }
    return 0;
}
