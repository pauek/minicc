#include <iostream>
using namespace std;

#include "../lexer.hh"
#include "../utils.hh"
#include "command.hh"

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
        } catch (Error *e) {
            e->print(cerr);
            return 1;
        }
    }
    return 0;
}
