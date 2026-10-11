#include <iostream>
using namespace std;

#include "command.hh"
#include "lexer.hh"
#include "utils.hh"

int Help(const vector<string>& _) {
    constexpr const char *usage =
        "Usage: minicc [options...] [command]\n"
        "\n"
        "Simplified C++ compiler and interpreter for educational purposes.\n"
        "\n"
        "Commands:\n"
        "   lex          Lex a file\n"
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

int Lex(const vector<string>& args) {
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
        } catch (LexerError *e) {
            e->print(cerr);
            return 1;
        }
    }
    return 0;
}
