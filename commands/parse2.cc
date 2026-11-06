#include <iostream>
using namespace std;

#include "../hl-lexer.hh"
#include "../lexer.hh"
#include "../utils.hh"
#include "command.hh"

void parse_file(string filename, bool show_filename) {
    auto text = read_file(filename);

    if (show_filename) {
        cout << filename << ":" << endl;
    }

    Lexer lexer(text, filename);
    lexer.run();

    HLexer hl_lex(lexer);
    hl_lex.run();
    hl_lex.dump(cout);
}

int Parse2(Args args) {
    for (auto filename : args) {
        try {
            parse_file(filename, args.size() > 1);
        } catch (Error *e) {
            e->print(cerr);
            return 1;
        }
    }
    return 0;
}
